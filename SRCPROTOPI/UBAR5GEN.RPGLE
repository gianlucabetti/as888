000100090901     D*------------------
000200090901     D* PASSAGGIO PARAMETRI A PROCEDURE UBRTVTPO_RtvPorto...
000300090901     D*------------------
000400090901     D ds_FIAR5      e ds                  extname(FIAR500F) qualified inz
000500090901     D dAR5GEN       e ds                  qualified inz
000600090901     D UBAR5GEN        ds                  qualified inz
000700090901     D  pInAnno                            like(ds_FIAR5.AR5aas)
000800090901     D  pInLineaPar                        like(ds_FIAR5.AR5lnp)
000900090901     D  pInSerie                           like(ds_FIAR5.AR5nrs)
001000090901     D  pInNumSped                         like(ds_FIAR5.AR5nsp)
001100090901     D  pOutAr5Gen                         like(dAR5gen)
