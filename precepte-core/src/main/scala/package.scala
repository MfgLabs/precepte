package com.mfglabs

import scala.language.higherKinds


package object precepte {

  type PST0[C] = PStateBase[BaseEnv, BaseTags, C]

  object PST0 {
    def apply[C](span: Span, env: BaseEnv, path: Call.Path[BaseTags], value: C): PST0[C] = PStateBase(span, env, path, value)
  }

  type TCTX0[F[_], C] = TaggingContext[BaseTags, PST0[C], F]

}