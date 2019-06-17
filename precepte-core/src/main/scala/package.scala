/*
Copyright 2015 Mfg labs.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package com.mfglabs

package object precepte {

  type InstrumentStep[Ta, ManagedState, UnmanagedState, F[_]] =
    λ[α => (PState[Ta, ManagedState, UnmanagedState], F[α])] ~~> F

  object InstrumentStepFun {
    @inline def iso[T, M, U, F[_], G[_]](iso: F <~~> G)(
        f: InstrumentStep[T, M, U, F]): InstrumentStep[T, M, U, G] =
      new (InstrumentStep[T, M, U, G]) {
        def apply[A](stg: (PState[T, M, U], G[A])): G[A] =
          iso.to(f[A](stg._1 -> iso.from(stg._2)))
      }

    @inline def contraMapUnmanagedState[T, M, U, U2, F[_]](from: U2 => U)(
        f: InstrumentStep[T, M, U, F]): InstrumentStep[T, M, U2, F] =
      new (InstrumentStep[T, M, U2, F]) {
        def apply[A](stf: (PState[T, M, U2], F[A])): F[A] =
          f[A](stf._1.mapUnmanaged(from) -> stf._2)
      }
  }
}
