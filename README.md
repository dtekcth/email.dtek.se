# @dtek.se email aliases are currently not supported

This project is currently put on ice until we either choose revive the service or until we decide that we're never going to provide email aliases again.
Until then, there is not much to see here.


# email.dtek.se
Manage your dtek email alias

## Functionality
We want to be able to login to the site using our https://auth.dtek.se where regular users only see their own aliases while admins (such as dHack) can see and manage all email aliases.

Admins can (on all aliases):
- Add email aliases
- Remove email aliases
- Reset email alias passwords
- Change forward address
- Change alias name and domain

Regular users can (on their own aliases):
- Remove email aliases
- Change forward address
- Request new email aliases that will be approved by admins

## Setup instructions

This is the raw way to setup your development environment.

### Database

We run MySQL because legacy, so install MySQL and then run this:

```
echo 'create database email;' | mysql -u root
mysql -u root email < scripts/create-db.sql
```

### Haskell

Install `stack` and then run:

```
stack install
```

If you get any error or get stuck, please file an issue.
