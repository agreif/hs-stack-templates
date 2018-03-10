# Project specifics
   - yesod-postgres template
   - auth-hashdb
   - riot-js component-based UI library
   - UIkit front-end framework

# Screenshots

![Home](../../screenshots/home.png)


# Template installation
```
stack new my-project https://raw.githubusercontent.com/Greif-IT/hs-stack-templates/master/yesod-riot-auth.hsfiles
```

# Create PostgreSQL Database (ubuntu)
```
sudo su postgres -c 'createuser --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication my-project'
sudo su postgres -c 'createdb --encoding=UTF-8 --owner=my-project --template=template0 my-project'
```

# Run Yesod server
```
cd my-project
sh run_dev.sh
```
# Initialize admin user
only needed after the first start
```
curl http://localhost:3000/initdb/me@example.com
```

you can see the generated password in the yesod logs like
```
[Error] ###############################
[Error] admin login:         admin
[Error] admin password:      XpISNqkWAGFfrlryWxNzbtkvJzFkszVj
[Error] admin password hash: sha256|17|CPlYqS5DBuHSZRtbJjUasQ==|wptBAbVHgSSRnJJ+7X1P6PbsJcM+oZ+JaGjs1xVNJns=
[Error] admin email:         me@example.com
[Error] ###############################
```
thus the generated password of the 'admin' user in this sample is: XpISNqkWAGFfrlryWxNzbtkvJzFkszVj

# Generate new password hashes
normally you set the password on the browser while the server is running
but in case you have to reset them hard in the database
```
sh run_passwd.sh mySecretPassword78574
```

will generate the hash: sha256|17|sm/d6UdH7+nRXZt7bJaAeg==|HVpHzSBOHQPwZiqmfEHiCZZPZh4tFLQyEuDKsNdqcN0=

then you can update the apprpriate database row for the admin user or some other user

# Generate model code
if model definitions changed
```
cd my-project
sh run_gen.sh
```

# Original yesod README.md

## Database Setup

After installing Postgres, run:

```
createuser yesod-riot-auth --password yesod-riot-auth --superuser
createdb yesod-riot-auth
createdb yesod-riot-auth_test
```

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically be recompiled and redeployed to localhost.

## Tests

```
stack test --flag yesod-riot-auth:library-only --flag yesod-riot-auth:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
