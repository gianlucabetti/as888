000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300060628      * ?__________________________________________________                 */
000400060627     ffnlbl01l  if   e           k disk
000500160826     fazorg01l  if   e           k disk
000600160826     fTIDP300F  o    e             disk
000700160826      * ?__________________________________________________                 */
000800160826     d fnlg28ds      e ds
000900160826     d fnlg24ds      e ds
001000160826     D og143         e ds
001100160826      * ?__________________________________________________                 */
001200160826     d date_iso        s               d   datfmt(*iso)
001300160826     d date_eur        s               d   datfmt(*eur)
001400160826      * ?__________________________________________________                 */
001500160826     d kaas            s                   like(i28aas)
001600160826     d klnp            s                   like(i28lnp)
001700160826     d knrs            s                   like(i28nrs)
001800160826     d knsp            s                   like(i28nsp)
001900060628      * ?__________________________________________________                 */
002000160826     c                   seton                                        lr
002100160826      *
002200060628      * ?se circuito DpD aggancia il fiar4 tipo rek'I' per reperire PARCEL  */
002300160826     c                   z-add     i28aas        kaas
002400160826     c                   z-add     i28lnp        klnp
002500160826     c                   z-add     i28nrs        knrs
002600160826     c                   z-add     i28nsp        knsp
002700160826      * ?__________________________________________________                 */
002800060628      * ?ricerca spedizione sui legami per agganciare bolla originale       */
002900060628     c     kspe          chain     fnlbl01l
003000160826      * ?________      _________  __________                                */
003100060628     c                   if        %found(fnlbl01l)
003200160826      *
003300060628      * ?verifica su azorg se circuito DpD                                  */
003400060628     c     lbllpo        chain     azorg01l
003500160826      *
003600160826     c                   if        %found(azorg01l)
003700060628     C                   movel     orgde3        og143
003800060628     c                   z-add     LBLAAO        kaas
003900060628     c                   z-add     LBLLPO        klnp
004000060628     c                   z-add     LBLNRO        knrs
004100060628     c                   z-add     LBLNSO        knsp
004200160826      *
004300160826      * ?se circuito DpD prende il Parcel
004400060628     C     §OgNTW        ifeq      'DPD'
004500060628     c                   exsr      parcel
004600060628     c                   endif
004700160826      *
004800060628     c                   endif
004900060628     c                   endif
005000060628
005100160826     c                   RETURN
005200060628      * ?__________________________________________________                 */
005300160826     c     parcel        begsr
005400060628      * ?__________________________________________________                 */
005500160826      *
005600160826     c                   clear                   fnlg24ds
005700160826     c                   z-add     Kaas          i24aas
005800160826     c                   z-add     Klnp          i24lnp
005900160826     c                   z-add     Knrs          i24nrs
006000160826     c                   z-add     Knsp          i24nsp
006100160826     c*
006200160826     c                   call      'FNLG24R'
006300160826     C                   parm                    fnlg24ds
006400160826     c*
006500160826     c* C'è il PARCEL quindi è un DPD:
006600160826     c                   if         o24npr <> *blank
006700160826     c                   exsr      chk_x_write
006800160826     c                   end
006900160826
007000060628     c                   endsr
007100060628      * ?___________________________________________________                */
007200160826     c     chk_x_write   begsr
007300160826      * ?---------------------------------------------------                */
007400160826     C                   clear                   §OGDP1_LNA        4
007500160826     C                   clear                   §OGDP1_LNP        4
007600160826      *
007700160826      * ?verifica su azorg se la LNP e la LNA dell'ultima bolla             */
007800160826      * ?dirottata hanno codici DEPOT differenti x poter scrivere           */
007900160826      * ?lo SCAN 06                                                         */
008000160829     c     lblLAN        chain     azorg01l
008100160826     c                   if        %found(azorg01l)
008200160826     C                   movel     orgde3        og143
008300160826     C                   movel     §OGDP1        §OGDP1_LNA
008400160826     c                   end
008500160826      *
008600160826     c     i28LNP        chain     azorg01l
008700160826     c                   if        %found(azorg01l)
008800160826     C                   movel     orgde3        og143
008900160826     C                   movel     §OGDP1        §OGDP1_LNP
009000160826     c                   end
009100160826      *
009200160826     C                   if         §OGDP1_LNP <> *blank and
009300160826     C                              §OGDP1_LNA <> *blank and
009400160826     C                              §OGDP1_LNA <> §OGDP1_LNP
009500160826      *  scrive lo SCAN 06
009600160826     c                   exsr      SCAN_06
009700160826     c                   end
009800160826      *
009900160826     c                   endsr
010000160826      * ?___________________________________________________                */
010100160826     c     SCAN_06       begsr
010200160826      * ?---------------------------------------------------                */
010300160826      *
010400160826     c                   clear                   tidp3000
010500160826     c                   eval        DP3AAS =  i24aas
010600160826     c                   eval        DP3LNP =  i24lnp
010700160826     c                   eval        DP3NRS =  i24nrs
010800160826     c                   eval        DP3NSP =  i24nsp
010900160826     c                   eval        DP3PRN =  o24npr
011000160826     c                   eval        DP3EVD =    '06 '
011100160826     c                   eval        DP3CEV =    'DIR'
011200160826     c                   eval        DP3FLE =    i28lnp
011300160826     c                   eval        DP3DEV =    UDATE8
011400160826     c                   eval        DP3HEV =    UTIME4
011500160826     c                   eval        DP3DTSPE =  UDATE8
011600160826     c                   write     tidp3000
011700160826      *
011800160826     c                   endsr
011900160826      * ?___________________________________________________                */
012000160826     c     *inzsr        begsr
012100160826      * ?---------------------------------------------------                */
012200051202     c     *entry        plist
012300160826     c                   parm                    fnlg28ds
012400051130      * originale
012500051130     c     kspe          klist
012600160826     c                   kfld                    kaas
012700160826     c                   kfld                    klnp
012800160826     c                   kfld                    knrs
012900160826     c                   kfld                    knsp
013000060628      *
013100160826     C* Imposta la UDATE  da TIME
013200160826     C                   TIME                    W0140            14 0
013300160826     C                   MOVE      W0140         UDATE8            8 0
013400160826     C                   MOVEL     W0140         UTIME4            4 0
013500160826     C     *eur          MOVE      UDATE8        date_eur
013600160826     C                   MOVE      date_eur      date_iso
013700160826     C                   MOVEl     date_iso      UDATE8
013800160826     C*
013900060628     c                   endsr
014000060627      * ?_____________________________________________________________      */
