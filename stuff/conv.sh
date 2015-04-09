# Recover deleted emails from my mail repo
# (c) Robin Schroer <sulami@peerwire.org>
# ISC-Licensed

# Get all the deleted mails. We only get mails from /cur/, otherwise we would
# have duplicates, because they moved through /new/ and possibly also /tmp/. We
# also filter out the mails in the trash, because those moved through the inbox
# as well.
getitems() {
    git log --diff-filter=D --summary | grep "/cur/" | grep -v "Trash/cur/" | cut -d ' ' -f 5
}

# Get the commit an email was deleted in.
getrev() {
    git rev-list -n 1 HEAD -- "$1"
}

for d in $(getitems); do
    # For each deleted email, we first get the commit it was deleted in, and
    # then get the version of this email from just before that commit. This
    # should restore all email that has gone through the system.
    r=$(getrev "$d")
    git checkout ${r}~1 -- "$d"
done

