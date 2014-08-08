function has(x,m) {
  if (undefined === x[m]) {return false} else {return true}
  };

function tail(x) { return x.slice(1, x.length) };

function thunk(v) { return {
  env: [v],
  fun: function (stk, g) { return [stk, g[0]] } }
  };

function eats(stk, env, fun, clos) {
  if (clos.goes.length) {
    return clos.goes[0]([stk, {
      kind: "Eat",
      env: env,
      han: fun.han,
      eat: fun.eat,
      clos: {env: clos.env, goes: tail(clos.goes)}
      }], clos.env);
    } else {
    return fun(stk, env);
    }
  };

function use(stk, m) {
  var x = null; var g = null;
  while (stk) {
    if (has(m, "comm")) { switch (stk[1].kind) {
      case "Eat" :
        if (has(stk[1].han, "dlers") && has(stk[1].han.dlers, m.comm)) {
          x = {cont: m.cont};
          for (i=0; i < m.env.length; i++) { x = [m.env[i], x]; }
          x = stk[1].han.dlers[m.comm](x);
          x = eats(stk[0], x[0].concat(stk[1].env), x[1], stk[1].clos);
          stk = x[0]; m = x[1]; continue;
          }
        if (has(stk[1].han, "gabout") && has(stk[1].han.gabout, m.comm)) {
          x = stk[1].eat({yield: m.comm, cont: m.cont, env: m.env});
          x = eats(stk[0], x[0].concat(stk[1].env), x[1], stk[1].clos);
          stk = x[0]; m = x[1]; continue;
          }
      default:
        m.cont = [stk[1], m.cont]; stk = stk[0]; continue;
    } } else { switch (stk[1].kind) {
      case "Car" :
        x = stk[1].go([stk[0], {kind: "Cdr", car: m}], stk[1].env);
        stk = x[0]; m = x[1]; continue;
      case "Cdr" :
        m = [stk[1].car, m]; stk = stk[0]; continue;
      case "Fun" :
        if (has(m, "fun")) { // the fun case
          x = eats(stk[0], m.env, m.fun, stk[1].clos);
          stk = x[0]; m = x[1]; continue;
          } else if (has(m, "yield")) { // the yield case
            m = {comm: m.yield, cont: m.cont, env: m.env};
            stk = stk[0]; continue;
          } else if (has(m, "cont")) { // the continuation case
            g = stk[1].clos.env; x = stk[1].clos.goes[0];
            stk = stk[0]; m = m.cont;
            while (m) { stk = [stk, m[0]]; m = m[1]; };
            x = x(stk, g);
            stk = x[0]; m = x[1]; continue;
          } else {  // the atom case
          if (stk[1].clos.goes.length) {
            x = stk[1].clos.goes[0]([stk[0], { kind: "Eff",
              comm: m, env: [],
              clos: {env: stk[1].clos.env, goes: tail(stk[1].clos.goes)}
              }], stk[1].clos.env);
            stk = x[0]; m = x[1]; continue;          
            }
          stk = stk[0]; m = {comm: m, cont: null, env: []}; continue;
          }
      case "Eat" :
        if (has(stk[1].han, "dlers")) {} else { m = thunk(m) };
        x = stk[1].eat(m);
        x = eats(stk[0], x[0].concat(stk[1].env), x[1], stk[1].clos);
        stk = x[0]; m = x[1]; continue;
      case "Eff" :
        if (stk[1].clos.goes.length) {
          x = stk[1].clos.goes[0]([stk[0], { kind: "Eff",
            comm: stk[1].comm, env: [m].concat(stk[1].env),
            clos: {env: stk[1].clos.env, goes: tail(stk[1].clos.goes)}
            }], stk[1].clos.env);
          stk = x[0]; m = x[1]; continue;          
          } else {
          m = {comm: stk[1].comm, cont: null, env: [m].concat(stk[1].env)}; stk = stk[0];
          continue;
          }
      } } }
  return m;
  };
