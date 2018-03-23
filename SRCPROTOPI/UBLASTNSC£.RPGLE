000100111011       // -?Parametri x ubLastNSC = Reperimento ultimo Segnacollo usato della Serie?
000200111011     d ds_FNBLP      e ds                  extname(FNBLP00F)
000300111011     d                                     based(nullptr)
000400111011     d                                     qualified
000500111011      *
000600111011     d pInSerie        s                   like(ds_FNBLP.BLPnrs)  inz
000700111011     d pInLineaPar     s                   like(ds_FNBLP.BLPlnp)  inz
000800111018     d pInLeg3C_SKC    s           3200                           inz
000900111011     d pInAnno         s                   like(ds_FNBLP.BLPaas)  inz
001000111011     d pOutLastNSC     s                   like(ds_FNBLP.BLPncd)  inz
001100111011     d pOutLineaPar    s                   like(ds_FNBLP.BLPlnp)  inz
001200111011     d pOutAnno        s                   like(ds_FNBLP.BLPaas)  inz
001300111018     d pOutMmGg        s                   like(ds_FNBLP.BLPmgs)  inz
001400111011     d pOutSpediz      s                   like(ds_FNBLP.BLPnsp)  inz
