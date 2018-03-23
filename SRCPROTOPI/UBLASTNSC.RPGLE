000100140225       // -?Parametri x ubLastNSC = Reperimento ultimi Segnacolli usati della Serie?
000200111011     d ds_FNBLP      e ds                  extname(FNBLP00F)
000300111011     d                                     based(nullptr)
000400111011     d                                     qualified
000500111011      *
000600111011     d pInSerie        s                   like(ds_FNBLP.BLPnrs)  inz
000700121121     d pInFilSgncl     s                   like(ds_FNBLP.BLPfls)  inz
000800111018     d pInLeg3C_SKC    s           3200                           inz
000900111011     d pInAnno         s                   like(ds_FNBLP.BLPaas)  inz
001000140225     d pOutMaxNSC      s                   like(ds_FNBLP.BLPncd)  inz
001100140225     d pOutMaxLNP      s                   like(ds_FNBLP.BLPlnp)  inz
001200140225     d pOutMaxAAS      s                   like(ds_FNBLP.BLPaas)  inz
001300140225     d pOutMaxMGS      s                   like(ds_FNBLP.BLPmgs)  inz
001400140225     d pOutMaxNSP      s                   like(ds_FNBLP.BLPnsp)  inz
001500140225     d pOutLastNSC     s                   like(ds_FNBLP.BLPncd)  inz
001600140225     d pOutLastLNP     s                   like(ds_FNBLP.BLPlnp)  inz
001700140225     d pOutLastAAS     s                   like(ds_FNBLP.BLPaas)  inz
001800140225     d pOutLastMGS     s                   like(ds_FNBLP.BLPmgs)  inz
001900140225     d pOutLastNSP     s                   like(ds_FNBLP.BLPnsp)  inz
