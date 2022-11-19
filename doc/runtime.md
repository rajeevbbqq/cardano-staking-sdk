# CTL's Runtime

Contracts written with CTL require access to several services at runtime. This guide will explain how to obtain and set up the services required for the version of CTL that we are using, along with how to configure CTL contracts to access them.

CTL officially bundles this runtime using Nix and docker-compose. It is not necessary, however, to use this bundle, and all services can be run independently (with some caveats, as explained below). If you would like to experiment with using this runtime, which bundles all required components into a single docker-compose environment, you can try experimenting with it by visiting the root of this repository and running `nix run .#frontend-runtime`.

As previously discussed, we will upgrade CTL for you once we have compatibility with the upcoming Vasil hardfork. This document will be updated to reflect any changes in CTL's runtime dependencies accordingly.

**Note**: Complete deployment strategies are beyond the scope of this guide. For example, you will probably wish to deploy different components using a reverse proxy and TLS. CTL makes no assumptions about this (see [configuring CTL's execution environment](#configuring-ctl-contract-execution) below).

**Table of Contents**

- [Prerequisites](#prerequisites)
- [Services](#services)
  - [`cardano-node`](#cardano-node)
    - [Version](#version)
    - [Installation](#installation)
    - [Configuration](#configuration)
    - [Links](#links)
  - [`ogmios`](#ogmios)
    - [Version](#version-1)
    - [Installation](#installation-1)
    - [Configuration](#configuration-1)
    - [Links](#links-1)
  - [`ogmios-datum-cache`](#ogmios-datum-cache)
    - [Version](#version-2)
    - [Installation](#installation-2)
    - [Configuration](#configuration-2)
    - [Links](#links-2)
  - [`ctl-server`](#ctl-server)
    - [Version](#version-3)
    - [Installation](#installation-3)
    - [Configuration](#configuration-3)
    - [Links](#links-3)
- [Configuring CTL contract execution](#configuring-ctl-contract-execution)

## Prerequisites

Make sure that you have installed and configured [Nix](./nix.md) as described in the project documentation.

## Services

### `cardano-node`

#### Version

`v1.35.3` is required.

#### Installation

If you do not already have a node running, we suggest using [Docker](https://hub.docker.com/r/inputoutput/cardano-node) to run the node. Make sure that you mount the node's IPC volume so it can be accessed from the host filesystem, e.g:

```
docker run --rm \
  -e NETWORK=testnet \
  -v "$PWD"/node/socket:/ipc \
  -v "$PWD"/node/data:/data \
  inputoutput/cardano-node:1.35.0
```

#### Configuration

No special configuration is required, beyond making sure that the node's IPC socket is exposed to Ogmios (see below). This requires setting the `CARDANO_NODE_SOCKET_PATH` environment variable in most cases. Following the example Docker invocation above:

```
export CARDANO_NODE_SOCKET_PATH="$PWD"/node/socket/node.socket
```

Make sure to either run the Docker container as a non-root user, or deploy `chmod` it to make it readable to external services.

#### Links

https://hub.docker.com/r/inputoutput/cardano-node
https://github.com/input-output-hk/cardano-node

### `ogmios`

#### Version

Revision `9c04524d45de2c417ddda9e7ab0d587a54954c57`

#### Installation

Ogmios can be installed/used in several ways. You can use upstream's [Docker](https://hub.docker.com/r/cardanosolutions/ogmios) image. The maintainers' preferred Docker use-case involves a package docker-compose configuration with an attached Cardano node, however.

You can also use the MLabs' [fork](https://github.com/mlabs-haskell/ogmios) of Ogmios and build it with Nix and then run it as a normal executable:

```
nix build .

./result/bin/ogmios
    --host <HOST> \
    --port <PORT> \
    --node-socket <SOCKET_PATH>/socket \
    --node-config <CONFIG_PATH>/cardano-node/config.json
```

#### Configuration

Ogmios requires access to the node's IPC socket. This means it must be run on the same host as the node and the socket must be writable by the Ogmios process.

You must also specify the network configurations to use with Ogmios. You can find IOHK's published configurations [here](https://github.com/input-output-hk/cardano-configurations). Then, select the desired network and pass the entire path to Ogmios, e.g.:

```
~/cardano-configurations/network/testnet/cardano-node/config.json
```

You can also specify the host and port for Ogmios to run on (the default is `localhost:1337`).

#### Links

https://github.com/CardanoSolutions/ogmios
https://github.com/mlabs-haskell/ogmios
https://ogmios.dev
https://github.com/input-output-hk/cardano-configurations

### `ogmios-datum-cache`

#### Version

Revision `880a69a03fbfd06a4990ba8873f06907d4cd16a7`

#### Installation

First, clone the [repository](https://github.com/mlabs-haskell/ogmios-datum-cache), then:

```
git checkout 880a69a03fbfd06a4990ba8873f06907d4cd16a7

nix build .
```

This will place the built binary in `./result/bin/ogmios-datum-cache`.

#### Configuration

A running Postgres instance is required. For the examples below, we will use the placeholder values of `user`, `password`, and `dbname` for the Postgres instance.

You can use an existing Postgres instance if available, or you can use Docker:

```
docker run -d --rm \
    -e "POSTGRES_USER=user" \
    -e "POSTGRES_PASSWORD=password" \
    -e "POSTGRES_DB=dbname" \
    -p 127.0.0.1:5432:5432 \
    postgres:13
```

ODC can be configured using command-line arguments, e.g.

```
ogmios-datum-cache
  --server-api '' \
  --server-port 9999 \
  --ogmios-address 127.0.0.1 \
  --ogmios-port 1337 \
  --db-port 5432 \
  --db-host localhost \
  --db-user user \
  --db-name dbname \
  --db-password password \
  --block-slot 54066900 \
  --block-hash 6eb2542a85f375d5fd6cbc1c768707b0e9fe8be85b7b1dd42a85017a70d2623d \
  --block-filter ''
```

The `--server-*` options above refer to configuration options for ODC itself; the `--server-api` option for control tokens can be ignored as we do not make use of that feature in our CTL contracts. Make sure that the `ogmios` options correspond to those that you start the Ogmios itself with.

#### Links

https://github.com/mlabs-haskell/ogmios-datum-cache

### `ctl-server`

#### Version

The version of `ctl-server` corresponds to the CTL tag/revision being used, currently `767e1f35b8b1f243f31ed3360d85a59ef695262b`, and the code resides in the same repository as CTL.

#### Installation

```
git checkout 767e1f35b8b1f243f31ed3360d85a59ef695262b

nix build .#ctl-server:exe:ctl-server
```

The binary will be under `./result/bin/ctl-server`

#### Configuration

CTL server can also be configured using command-line parameters. Currently, only one option is available:

```

ctl-server --port <PORT>

```

#### Links

https://github.com/Plutonomicon/cardano-transaction-lib

## Configuring CTL contract execution

CTL requires information about each of these runtime dependencies in order to execute contracts. This can be done using the `SdkConfig` type that is provided when creating each type of pool. The following example assumes that the services have been deployed using a dedicated HTTP server (e.g. Nginx) with TLS certificates and a reverse proxy (note: such a setup is **not** required for CTL contract execution).

```javascript
const sdkConfig = {
  ctlServerConfig: {
    host: "example.com",
    port: 443,
    secure: true,
    path: "",
  },
  ogmiosConfig: {
    host: "example.com",
    port: 443,
    secure: true,
    path: "",
  },
  datumCacheConfig: {
    host: "example.com",
    port: 443,
    secure: true,
    path: "",
  },
  networkId: 1,
  logLevel: "Error",
};

const bondedPoolAdminExample = async () => {
  const initialArgs = {
    /* omitted for brevity */
  };
  const bondedPool = await createBondedPool(sdkConfig, initialArgs);
  await bondedPool.deposit();
  // ...
};
```
