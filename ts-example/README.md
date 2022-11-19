# `singularitynet` Typescript example

This subdirectory contains a new Typescript project demonstrating how to consume the [Javascript SDK](../package.json) that we created. This document outlines the necessary steps to build and bundle the main JS SDK and then build the Typescript project. It also details some important details for your own frontend integration.

**Table of Contents**

- [How to build and run](#how-to-build-and-run)
  - [Build the main SDK package](#build-the-main-sdk-package)
  - [Build the Typescript project](#build-the-typescript-project)
- [Other important notes](#other-important-notes)
  - [Type definitions](#type-definitions)
  - [Creating the pools](#creating-the-pools)
    - [Changing the service configurations](#changing-the-service-configurations)
  - [Using `big-integer` instead of native `bigint`s](#using-big-integer-instead-of-native-bigints)
  - [The `BROWSER_RUNTIME` environment variable](#the-browser_runtime-environment-variable)
  - [Webpack](#webpack)

## How to build and run

### Build the main SDK package

We will simulate pulling the main SDK package from NPM by using the SDK directory (i.e. `frontend`) as a local NPM dependency.

_`frontend/ts-example/package.json`_:

```json-with-comments
{
  "name": "singularitynet-example",
  // Omitted for brevity
  "dependencies": {
    "singularitynet": "file:../"
  }
}
```

Since we haven't published the package, we need to build it first using the following steps:

1. Enter the root of the repository
2. Enter the Nix development environment and then the `frontend` directory:
   ```
   $ nix develop .#frontend
   $ cd frontend
   ```
3. Build the SDK
   ```
   $ npm run js:build
   ```

This will output the built package into `dist`. It has been made available under the library name of `singularitynet` and can be imported by package consumers using this name (see the SDK's [Webpack configuration](../webpack.config.js) under `output.library.name`).

### Build the Typescript project

We're now ready to consume the (local) SDK package. If you make any changes in the `frontend` directory, you can easily repeat the previous step to re-build it and update it as a dependency for the TS project.

1. Enter the `frontend/ts-example` directory
2. Install the NPM dependencies to the local directory, including the `singularitynet` package
   ```
   $ npm i
   ```
   **Note**: We do **not** do this before building the main SDK package as the NPM dependencies are handled by Nix in that case. For the current TS example, however, I did not create a similar setup so we need to use `npm` directly.
3. If you're running the CTL runtime on `localhost`, make sure that's started. Otherwise, change the configurations in `index.ts` (see [below](#changing-the-service-configurations))
4. Either start the development server or run the build:
   ```
   $ npm run dev
   ```
   And visit `localhost:4008` in your browser, _or_
   ```
   $ npm run build
   ```

## Other important notes

This section outlines some things you should be aware of when consuming the main JS SDK. There are examples of each in this TS example. When writing your own JS/TS modules, please take all of these considerations into account.

### Type definitions

The present example makes use of several type definitions, but there are several more defined for the SDK. Have a look at the TS [declarations](../index.d.ts) for the main SDK to see all of the defined types. All of these can be `import`ed normally in your own integrations, e.g.

```typescript
import {
  SdkConfig,
  LogLevel,
  SdkServerConfig,
  BondedPool,
} from "singularitynet";
```

### Creating the pools

`index.d.ts` defines two immutable classes that represent bonded and unbonded pools (`BondedPool` and `UnbondedPool`, respectively). **Do not** use the constructors directly. Instead, use the asynchronous `createBondedPool` and `createUnbondedPool` functions. This is because the initializations of each pool must be done asynchronously and JS does not allow `async` constructors. For example

```typescript
const main = async () => {
  const pool: BondedPool = await createBondedPool(
    someSdkConfig,
    someInitialArgs
  );
  await pool.deposit(BigInteger(1), []);
  // more pool operations...
};
```

#### Changing the service configurations

Both types of pools take an `SdkConfig` as their first argument which includes information for the contracts to connect to the required runtime services (`ogmios`, `ctl-server`, etc...). For the purposes of this TS example, we're using `localhost` for all services:

_`frontend/ts-example/index.ts`_

```typescript
import { SdkConfig } from "singularitynet";

// omitted for brevity

const localHostSdkConfig: SdkConfig = {
  ctlServerConfig: {
    host: "localhost",
    port: 8081,
    secure: false,
    path: "",
  },
  ogmiosConfig: {
    host: "localhost",
    port: 1337,
    secure: false,
    path: "",
  },
  datumCacheConfig: {
    host: "localhost",
    port: 9999,
    secure: false,
    path: "",
  },
  networkId: 1,
  logLevel: "Info",
};
```

You can easily change all of these to connect to remote services, however. For example, we could host all of them on `some-domain.com` with TLS enabled and route them to different paths. We would only need to change the `SdkConfig` to support this:

```typescript
const remoteSdkConfig: SdkConfig = {
  ctlServerConfig: {
    host: "some-domain.com",
    port: 443,
    secure: true,
    path: "ctl",
  },
  ogmiosConfig: {
    host: "some-domain.com",
    port: 443,
    secure: true,
    path: "ogmios",
  },
  datumCacheConfig: {
    host: "some-domain.com",
    port: 443,
    secure: true,
    path: "odc",
  },
  networkId: 1,
  logLevel: "Error",
};
```

### Using `big-integer` instead of native `bigint`s

Because we use a Purescript package that depends on `big-integer`, an NPM package which extends the `bigint` type with additional methods, you **cannot** use `bigint`s or the `BigInt` constructor directly. The correct approach is illustrated by the following:

```typescript
import { BigInteger } from "big-integer";
import { InitialBondedArgs } from "singularitynet";

const BigInteger = require("big-integer");

const initialBondedArgs: InitialBondedArgs = {
  iterations: BigInteger(2), // NOT `BigInt`
  // omitted for brevity
};

const someFn = (
  x: BigInteger // NOT `bigint`
) => {};
```

### The `BROWSER_RUNTIME` environment variable

CTL depends directly on `cardano-serialization-lib` which publishes several incompatible packages for different environments (the browser, NodeJS, ASM.js, etc...). It isn't possible to polyfill these, so the CTL authors have introduced a `BROWSER_RUNTIME` environment variable to determine the environment and load the correct package.

In our case, we **always** want to select the `-browser` package, so we must set `BROWSER_RUNTIME=1` when building (that is, when invoking Webpack). This is illustrated by the following:

_`frontend/ts-example/package.json`_:

```json-with-comments
{
  "name": "singularitynet-example",
  "scripts": {
    "dev": "BROWSER_RUNTIME=1 webpack-dev-server --mode=development",
    "build": "BROWSER_RUNTIME=1 webpack --mode=production"
  }
  // Omitted for brevity
}
```

It is also necessary to add this as a plugin in our Webpack configurations so it is always passed through, for example

_`frontend/ts-example/webpack.config.js`_:

```javascript
const webpack = require("webpack");

module.exports = {
  // omitted for brevity
  plugins: [
    new webpack.DefinePlugin({
      BROWSER_RUNTIME: !!process.env.BROWSER_RUNTIME,
    }),
    // other plugins here
  ];
};
```

**Make sure** to do the same when writing your own integrations!

### Webpack

For greatest comptability, we recommend using the same version of Webpack (`v5.67.0`) as we do, with the same `experiments` enabled. This can be fragile due to our somewhat unique circumstances (our dependencies needing to load WASM using top-level `await`). The relevant `experiments` include

_`frontend/ts-example/webpack.config.js`_:

```javascript
module.exports = {
  experiments: {
    asyncWebAssembly: false,
    layers: false,
    lazyCompilation: false,
    outputModule: true,
    syncWebAssembly: true,
    topLevelAwait: true,
  },
  // omitted for brevity
};
```
