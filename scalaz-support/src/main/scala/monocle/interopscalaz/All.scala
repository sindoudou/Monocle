package monocle.interopscalaz

object all extends ScalazInteropInstances

trait ScalazInteropInstances
  extends DisjunctionOptics
  with    Either3Optics
  with    IListOptics
  with    IMapOptics
  with    ISetOptics
  with    MaybeOptics
  with    NonEmptyListOptics
  with    OneAndOptics
  with    TheseOptics
  with    TreeOptics
  with    ValidationOptics
