# Pure source-time image metadata. No I/O, no network, no wall-clock.
# `created` is flake `self.lastModifiedDate` (YYYYMMDDHHMMSS UTC)
# reformatted to RFC 3339. Identical sources yield identical bytes.
# Missing revision metadata fails closed: never emit "dev" or wall-clock.
{
  self,
  sourceUrl,
}:
let
  inherit (builtins) stringLength substring;
  raw = self.lastModifiedDate or
    (builtins.throw "image-meta: flake self is missing lastModifiedDate");
  created =
    if stringLength raw < 14 then
      builtins.throw "image-meta: lastModifiedDate is shorter than YYYYMMDDHHMMSS"
    else
      substring 0 4 raw
      + "-"
      + substring 4 2 raw
      + "-"
      + substring 6 2 raw
      + "T"
      + substring 8 2 raw
      + ":"
      + substring 10 2 raw
      + ":"
      + substring 12 2 raw
      + "Z";
  revision = self.rev or self.dirtyRev or
    (builtins.throw "image-meta: flake self has no rev or dirtyRev");
  version = self.dirtyShortRev or self.shortRev or
    (builtins.throw "image-meta: flake self has no shortRev or dirtyShortRev");
  labels = {
    "org.opencontainers.image.created" = created;
    "org.opencontainers.image.revision" = revision;
    "org.opencontainers.image.source" = sourceUrl;
    "org.opencontainers.image.version" = version;
  };
in
{
  inherit created revision version labels;
}
