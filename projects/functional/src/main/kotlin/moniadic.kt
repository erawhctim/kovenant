/*
 * Copyright (c) 2015 Mark Platvoet<mplatvoet@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * THE SOFTWARE.
 */
@file:JvmName("KovenantFnMoniadic")
package nl.komponents.kovenant.functional

import nl.komponents.kovenant.*

/**
 * Asynchronously map the success value of a [Promise] and returns a new [Promise] with the transformed value.
 *
 * Transforms Promise A to Promise B. If Promise A resolves successful then [fn] is executed on the
 * work [DispatcherContext] of the default `Kovenant.context` and returns Promise B. If [fn] is successful,
 * meaning now Exception is thrown then Promise B resolves successful, failed otherwise.
 *
 * If Promise A fails with error E, Promise B will fail with error E too.
 *
 * @param fn the transform function.
 */
infix fun <V, R> Promise<V, Exception>.map(fn: (V) -> R): Promise<R, Exception> = then(fn)

infix fun <V, R, E: Exception> Promise<V, E>.mapE(fn: (V) -> R): Promise<R, E> = thenE(fn)

/**
 * Asynchronously map the success value of a [Promise] and returns a new [Promise] with the transformed value.
 *
 * Transforms Promise A to Promise B. If Promise A resolves successful then [fn] is executed on the
 * work [DispatcherContext] of the default `Kovenant.context` and returns Promise B. If [fn] is successful,
 * meaning now Exception is thrown then Promise B resolves successful, failed otherwise.
 *
 * If Promise A fails with error E, Promise B will fail with error E too.
 *
 * @param context the on which the map function and returned Promise operate
 * @param bind the transform function.
 */
fun <V, R> Promise<V, Exception>.map(context: Context, bind: (V) -> R): Promise<R, Exception> = then(context, bind)

fun <V, R, E: Exception> Promise<V, E>.mapE(context: Context, bind: (V) -> R): Promise<R, E> = thenE(context, bind)


/**
 * Asynchronously bind the success value of a [Promise] and returns a new [Promise] with the transformed value.
 *
 * Transforms Promise A to Promise B with a bind function that returns Promise B.
 * If Promise A resolves successful then [fn] is executed on the
 * work [DispatcherContext] of the default `Kovenant.context` and returns Promise B. If [bind] is successful,
 * meaning now Exception is thrown then Promise B resolves successful, failed otherwise.
 *
 * If Promise A fails with error E, Promise B will fail with error E too.
 *
 * @param bind the transform function.
 */
infix fun <V, R> Promise<V, Exception>.bind(fn: (V) -> Promise<R, Exception>): Promise<R, Exception> = bind(context, fn)

infix fun <V, R, E: Exception> Promise<V, E>.bindE(fn: (V) -> Promise<R, E>): Promise<R, E> = bindE(context, fn)

/**
 * Asynchronously bind the success value of a [Promise] and returns a new [Promise] with the transformed value.
 *
 * Transforms Promise A to Promise B with a bind function that returns Promise B.
 * If Promise A resolves successful then [bind] is executed on the
 * work [DispatcherContext] of the default `Kovenant.context` and returns Promise B. If [bind] is successful,
 * meaning now Exception is thrown then Promise B resolves successful, failed otherwise.
 *
 * If Promise A fails with error E, Promise B will fail with error E too.
 *
 * @param context the on which the bind and returned Promise operate
 * @param bind the transform function.
 */
fun <V, R> Promise<V, Exception>.bind(context: Context, fn: (V) -> Promise<R, Exception>): Promise<R, Exception> {
    if (isDone()) when {
        isSuccess() -> {
            val deferred = deferred<R, Exception>(context)
            bindAsync(fn, context, deferred, get())
            return deferred.promise
        }
        isFailure() -> return Promise.ofFail(getError(), context)
    }

    val deferred = deferred<R, Exception>(context)
    success {
        value ->
        bindAsync(fn, context, deferred, value)
    }
    fail {
        deferred reject it
    }

    return deferred.promise
}

fun <V, R, E: Exception> Promise<V, E>.bindE(context: Context, fn: (V) -> Promise<R, E>): Promise<R, E> {
    if (isDone()) when {
        isSuccess() -> {
            val deferred = deferred<R, E>(context)
            bindAsyncE(fn, context, deferred, get())
            return deferred.promise
        }
        isFailure() -> return Promise.ofFail(getError(), context)
    }

    val deferred = deferred<R, E>(context)
    success {
        value ->
        bindAsyncE(fn, context, deferred, value)
    }
    fail {
        deferred reject it
    }

    return deferred.promise
}

private fun <R, V> bindAsync(bind: (V) -> Promise<R, Exception>,
                             context: Context,
                             deferred: Deferred<R, Exception>,
                             value: V) {
    context.workerContext.offer {
        try {
            val p = bind(value)
            p success { deferred resolve it }
            p fail { deferred reject it }
        } catch (e: Exception) {
            //just like map/then consider bind exception as rejection
            deferred reject e
        }
    }
}

private fun <R, V, E: Exception> bindAsyncE(bind: (V) -> Promise<R, E>,
                                           context: Context,
                                           deferred: Deferred<R, E>,
                                           value: V) {
    context.workerContext.offer {
        try {
            val p = bind(value)
            p success { deferred resolve it }
            p fail { deferred reject it }
        } catch (e: Exception) {
            //just like map/then consider bind exception as rejection
            deferred reject e as E
        }
    }
}

/**
 * Applies the map function of the provided `Promise` to the result of this `Promise` and returns a new `Promise` with
 * the transformed value.
 *
 * If either this or the provided `Promise` fails the resulting `Promise` has failed too. this `Promise` takes
 * precedence over the provided `Promise` if both fail.
 *
 * @param promise Promise containing the map function
 */
infix fun <V, R> Promise<V, Exception>.apply(promise: Promise<(V) -> R, Exception>): Promise<R, Exception> {
    return this.apply(this.context, promise)
}

infix fun <V, R, E: Exception> Promise<V, E>.applyE(promise: Promise<(V) -> R, E>): Promise<R, E> {
    return this.applyE(this.context, promise)
}


/**
 * Applies the map function of the provided `Promise` to the result of this `Promise` and returns a new `Promise` with
 * the transformed value.
 *
 * If either this or the provided `Promise` fails the resulting `Promise` has failed too. this `Promise` takes
 * precedence over the provided `Promise` if both fail.
 *
 * @param context the context on which the map function and the returned promise operate.
 * @param promise Promise containing the map function
 */
fun <V, R> Promise<V, Exception>.apply(context: Context, promise: Promise<(V) -> R, Exception>): Promise<R, Exception> {
    if (isDone()) when {
        isDone() -> {
            val deferred = deferred<R, Exception>(context)
            applyAsync(promise, context, deferred, get())
            return deferred.promise
        }
        isFailure() -> return Promise.ofFail(getError(), context)
    }

    val deferred = deferred<R, Exception>(context)
    success {
        value ->
        applyAsync(promise, context, deferred, value)
    }
    fail { deferred reject it }

    return deferred.promise
}

fun <V, R, E: Exception> Promise<V, E>.applyE(context: Context, promise: Promise<(V) -> R, E>): Promise<R, E> {
    if (isDone()) when {
        isDone() -> {
            val deferred = deferred<R, E>(context)
            applyAsyncE(promise, context, deferred, get())
            return deferred.promise
        }
        isFailure() -> return Promise.ofFail(getError(), context)
    }

    val deferred = deferred<R, E>(context)
    success {
        value ->
        applyAsyncE(promise, context, deferred, value)
    }
    fail { deferred reject it }

    return deferred.promise
}

private fun <R, V> applyAsync(promise: Promise<(V) -> R, Exception>, context: Context, deferred: Deferred<R, Exception>, value: V) {
    if (promise.isDone()) when {
        promise.isSuccess() -> return applyAsync(context, deferred, promise.get(), value)
        promise.isFailure() -> return deferred reject promise.getError()
    }

    promise success {
        fn ->
        applyAsync(context, deferred, fn, value)
    }
    promise fail { deferred reject it }
}

private fun <R, V, E: Exception> applyAsyncE(promise: Promise<(V) -> R, E>, context: Context, deferred: Deferred<R, E>, value: V) {
    if (promise.isDone()) when {
        promise.isSuccess() -> return applyAsyncE(context, deferred, promise.get(), value)
        promise.isFailure() -> return deferred reject promise.getError()
    }

    promise success {
        fn ->
        applyAsyncE(context, deferred, fn, value)
    }
    promise fail { deferred reject it }
}

private fun <R, V> applyAsync(context: Context, deferred: Deferred<R, Exception>, fn: (V) -> R, value: V) {
    context.workerContext.offer {
        try {
            deferred resolve fn(value)
        } catch (e: Exception) {
            deferred reject e
        }
    }
}

private fun <R, V, E: Exception> applyAsyncE(context: Context, deferred: Deferred<R, E>, fn: (V) -> R, value: V) {
    context.workerContext.offer {
        try {
            deferred resolve fn(value)
        } catch (e: Exception) {
            deferred reject e as E
        }
    }
}