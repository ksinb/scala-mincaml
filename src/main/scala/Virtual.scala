package mincaml

object Virtual extends Asm{
  var data = List[(Id.L, Double)]()

  object separate {
    type TA = Id.T
    type TB = (List[TA], List[TA])

    def classify(xts:List[(TA, Type.T)], ini:TB, addf:(TB, TA)=>TB, addi:(TB, TA, Type.T)=>TB):TB = {
      xts.foldLeft(ini){
        case (acc, (x, t)) =>
          t match {
            case Type.Unit() => acc
            case Type.Float() => addf(acc, x)
            case _ => addi(acc, x, t)
          }
      }
    }

    def apply(xts:List[(TA, Type.T)]):TB = {
      classify(
        xts,
        (List[TA](), List[TA]()),
        {case ((int1, float1), x) => (int1, float1:::List(x))},
        {case ((int2, float2), x, _) => (int2:::List(x), float2)}
      )
    }
  }

  object expand{
    type TA = Id.T
    type TB = (Int, T)

    def classify(xts:List[(TA, Type.T)], ini:TB, addf:(TB, TA)=>TB, addi:(TB, TA, Type.T)=>TB):TB = {
      xts.foldLeft(ini) {
        case (acc, (x, t)) =>
          t match {
            case Type.Unit() => acc
            case Type.Float() => addf(acc, x)
            case _ => addi(acc, x, t)
          }
      }
    }

    def apply(xts:List[(TA, Type.T)], ini:TB, addf:(TA, Int, T)=>T, addi:(TA, Type.T, Int, T)=>T):TB = {
      classify(
        xts,
        ini,
        {case ((offset, acc), x) =>
          val ofs = align(offset)
          (ofs+8, addf(x, ofs, acc))
        },
        {case ((offset, acc), x, t) =>
          (offset+4, addi(x, t, offset, acc))
        }
      )
    }
  }

  def g(env:Map[Id.T, Type.T], e:Closure.T):T = {
    e match {
      case Closure.Unit() => Ans(Nop())
      case Closure.Int(i) => Ans(Set(i))

      case Closure.Float(d) =>
        val v = data find {case (_, dp) => dp == d}
        val l = v match {
          case Some((n, _)) => n
          case None =>
            val idl = new Id.L(Id.genid("l"))
            data =  (idl, d) :: data
            idl
        }
        val x = Id.genid("l")
        Let((x, Type.Int()), SetL(l), Ans(LdDF(x, C(0), 1)))

      case Closure.Neg(x) => Ans(Neg(x))
      case Closure.Add(x, y) => Ans(Add(x, V(y)))
      case Closure.Sub(x, y) => Ans(Sub(x, V(y)))

      case Closure.FNeg(x) => Ans(FNegD(x))
      case Closure.FAdd(x, y) => Ans(FAddD(x, y))
      case Closure.FSub(x, y) => Ans(FSubD(x, y))
      case Closure.FMul(x, y) => Ans(FMulD(x, y))
      case Closure.FDiv(x, y) => Ans(FDivD(x, y))

      case Closure.IfEq(x, y, e1, e2) =>
        env(x) match {
          case Type.Bool() =>  Ans(IfEq(x, V(y), g(env, e1), g(env, e2)))
          case Type.Int() =>  Ans(IfEq(x, V(y), g(env, e1), g(env, e2)))
          case Type.Float() =>  Ans(IfFEq(x, y, g(env, e1), g(env, e2)))
          case _ => throw new Exception("IfEq: equality supported only for bool, int, and float")
        }
      case Closure.IfLE(x, y, e1, e2) =>
        env(x) match {
          case Type.Bool() =>  Ans(IfLE(x, V(y), g(env, e1), g(env, e2)))
          case Type.Int() =>  Ans(IfLE(x, V(y), g(env, e1), g(env, e2)))
          case Type.Float() =>  Ans(IfFLE(x, y, g(env, e1), g(env, e2)))
          case _ => throw new Exception("IfLE: equality supported only for bool, int, and float")
        }

      case Closure.Let((x, t1), e1, e2) =>
        val e1p = g(env, e1)
        val e2p = g(env+(x->t1), e2)
        concat(e1p, (x, t1), e2p)

      case Closure.Var(x) =>
        env(x) match {
          case Type.Unit() => Ans(Nop())
          case Type.Float() => Ans(FMovD(x))
          case _ => Ans(Mov(x))
        }

      case Closure.MakeCls((x, t), Closure.Closure(l, ys), e2) =>
        val e2p = g(env+(x->t), e2)
        val (offset, store_fv) = expand(
            ys.map(y=>(y, env(y))),
            (4, e2p),
            (y, offset, store_fv) => seq(StDF(y, x, C(offset), 1), store_fv),
            (y, _, offset, store_fv) => seq(St(y, x, C(offset), 1), store_fv)
        )
        val z = Id.genid("l")
        Let((x, t), Mov(reg_hp),
          Let((reg_hp, Type.Int()), Add(reg_hp, C(align(offset))),
            Let((z, Type.Int()), SetL(l),
              seq(St(z, x, C(0), 1), store_fv))))

      case Closure.AppCls(x, ys) =>
        val (int, float) = separate(ys.map(y=>(y, env(y))))
        Ans(CallCls(x, int, float))

      case Closure.AppDir(x, ys) =>
        val(int, float) = separate(ys.map(y=>(y, env(y))))
        Ans(CallDir(x, int, float))

      case Closure.Tuple(xs) =>
        val y = Id.genid("t")
        val (offset, store) = expand(
          xs.map(x=>(x, env(x))),
          (0, Ans(Mov(y))),
          (x, offset, store) => seq(StDF(x, y, C(offset), 1), store),
          (x, _, offset, store) => seq(St(x, y, C(offset), 1), store)
        )
        Let(
          (y, Type.Tuple(xs.map(x=>env(x)))),
          Mov(reg_hp),
          Let(
            (reg_hp, Type.Int()),
            Add(reg_hp, C(align(offset))),
            store
          )
        )

      case Closure.LetTuple(xts, y, e2) =>
        val s = Closure.fv(e2)
        val (offset, load) = expand(
          xts,
          (0, g(env++xts, e2)),
          (x, offset, load) => if (s contains x) load else fletd(x, LdDF(y, C(offset), 1), load),
          (x, t, offset, load) => if (s contains x) load else Let((x, t), Ld(y, C(offset), 1), load)
        )
        load

      case Closure.Get(x, y) =>
        env(x) match {
          case Type.Array(Type.Unit()) => Ans(Nop())
          case Type.Array(Type.Float()) => Ans(LdDF(x, V(y), 8))
          case Type.Array(_) => Ans(Ld(x, V(y), 4))
          case _ => throw new Exception("false")
        }

      case Closure.Put(x, y, z) =>
        env(x) match {
          case Type.Array(Type.Unit()) => Ans(Nop())
          case Type.Array(Type.Float()) => Ans(StDF(z, x, V(y), 8))
          case Type.Array(_) => Ans(St(z, x, V(y), 4))
          case _ => throw new Exception("false")
        }

      case Closure.ExtArray(x) => Ans(SetL("mincaml_"+x))
    }
  }

  def h(fund:Closure.Fundef) = {
    fund match {
      case Closure.Fundef((x, t), yts, zts, e) =>
        val (int, float) = separate(yts)
        val (offset, load) = expand(
          zts,
          (4, g(Map()++zts++yts+(x->t), e)),
          (z, offset, load) => fletd(z, LdDF(x, C(offset), 1), load),
          (z, _, offset, load) => Let((z, t),  Ld(x, C(offset), 1), load)
        )
        t match {
          case Type.Fun(_, t2) => Fundef(x, int, float, load, t2)
          case _ => throw new Exception("false")
        }
    }
  }

  def f(prg:Closure.Prog):Prog = {
    prg match {
      case Closure.Prog(fd:List[Closure.Fundef], e:Closure.T) =>
        data = List[(Id.L, Double)]()
        val fundefs = fd.map(h)
        val ep = g(Map(), e)
        Prog(data, fundefs, ep)
    }
  }

  def apply(e:Closure.Prog) = f(e).asInstanceOf[Simm.Prog]
}
