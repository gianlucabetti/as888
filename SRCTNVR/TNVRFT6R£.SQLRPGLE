000100170504     H DECEDIT('0,') DATEDIT(*YMD.)
000200170505     ffiarbt0f  if   e             disk
000201170508     ffnarbk0f  if   e             disk
000202170508     ffnfta01l  if a e           k disk
000203170504
000204170508        read fiarbt0f ;
000205170508        dow not %eof(fiarbt0f);
000300170508           if ARBDFT>=20170101 and (arbcvb ='T6' or arbcvb='26');
000301170508               setll (arbFIV:arbnft:arbdft) fnfta01l ;
000302170508               if not %equal(fnfta01l);
000303170508                  exsr sr_fnfta;
000304170508               endif;
000400170508           endif;
000401170508           read fiarbt0f;
000500170504        enddo;
000501170508
000502170508        read fnarbk0f ;
000503170508        dow not %eof(fnarbk0f);
000504170508           if ARBDFT>=20170101 and arbcvb ='K8' and
000505170508               (arbcbn = 'Z ' or
000506170508                arbcbn = 'Y ' or
000507170508                arbcbn = 'P ' or
000508170508                arbcbn = 'I ');
000509170508               setll (arbFIV:arbnft:arbdft) fnfta01l ;
000510170508               if not %equal(fnfta01l);
000511170508                  exsr sr_fnfta;
000512170508               endif;
000513170508           endif;
000514170508           read fnarbk0f;
000515170508        enddo;
000516170508        *inlr = *on ;
000600170508        //-----------------------------------------------------------------------*
000700170508        // Memorizzazione dati fattura annullata/stornata
000800170508        //-----------------------------------------------------------------------*
000900170508        begsr sr_Fnfta     ;
000901170508        clear  fnfta000    ;
001000170508        FTAAAS = arbaas    ;
001100170508        FTALNP = arblnp    ;
001200170508        FTANRS = arbnrs    ;
001300170508        FTANSP = arbnsp    ;
001400170508        //FTAMGS = arbmgs    ;
001500170508        FTADIV = arbdiv    ;
001600170508        FTAALI = arbali    ;
001700170508        FTACEI = arbcei    ;
001800170508        FTAIFT = arbift    ;
001900170508        FTAFIV = arbfiv    ;
002000170508        FTANFT = arbnft    ;
002100170508        FTADFT = arbdft    ;
002200170508
002300170508        write(e) fnfta000    ;
002400170508        endsr;
