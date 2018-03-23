000100990504     H* ACTGRP=*CALLER
000200171020     H DEBUG DECEDIT('0,') DATEDIT(*YMD/)
000300171020      *  __________________________________________________
000400171020      *  Se restituisce il DEPOT a 4 più il depot a 7 significa che
000500171020      *  è andato a decodificare il depot a 7 dall'anagrafica del
000600171020      *  cappario nuovo DPD alrimenti restituisce solo il DEPOT a 7
000700171020      *  lasciando vuoto quello a 4
000800171020      *  __________________________________________________
000900060627     ffnlbl01l  if   e           k disk
001000171018      *--
001100171018      * tolto il vecchio AR4 e attivato il nuovo FIPND
001200171018     f***** fiar401l  if   e           k disk
001300171018     ffipnd01l  if   e           k disk
001400171018      *--
001500060627     fazorg01l  if   e           k disk
001600171020      *
001700060627     d fnlg24ds      e ds
001800060627     D og143         e ds
001900171018     D***** dsbl4i        e ds
002000060628     d kaas            s                   like(i24aas)
002100060628     d klnp            s                   like(i24lnp)
002200060628     d knrs            s                   like(i24nrs)
002300060628     d knsp            s                   like(i24nsp)
002400171020     D*------------------------------------------------------------------------*
002500171020      *  __________________________________________________                 */
002600171020      *  se circuito DpD aggancia il fiar4 tipo rek'I' per reperire PARCEL  */
002700060628     c                   z-add     i24aas        kaas
002800060628     c                   z-add     i24lnp        klnp
002900060628     c                   z-add     i24nrs        knrs
003000060628     c                   z-add     i24nsp        knsp
003100060628     c                   exsr      parcel
003200150714      *
003300150714     c                   if        o24npr = *blank
003400060628     c                   exsr      legami
003500060628     c                   endif
003600060628      *
003700000607     c                   seton                                        lr
003800171020      *  __________________________________________________                 */
003900060628     c     legami        begsr
004000171020      *  __________________________________________________                 */
004100171020      *  ricerca spedizione sui legami per agganciare bolla originale       */
004200060628     c     kspe          chain     fnlbl01l
004300060628     c                   if        %found(fnlbl01l)
004400171020      *  verifica su azorg se circuito DpD                                  */
004500060628     c     lbllpo        chain     azorg01l
004600060628     c                   if        %found(azorg01l)
004700060628     C                   movel     orgde3        og143
004800060628     c                   z-add     LBLAAO        kaas
004900060628     c                   z-add     LBLLPO        klnp
005000060628     c                   z-add     LBLNRO        knrs
005100060628     c                   z-add     LBLNSO        knsp
005200171020      *  se circuito DpD aggancia il fiar4 tipo rek'I' per reperire PARCEL  */
005300060628     C     §OgNTW        ifeq      'DPD'
005400060628     c                   exsr      parcel
005500060628     c                   endif
005600060628     c                   endif
005700060628     c                   endif
005800060628
005900060628     c                   endsr
006000171018      *  __________________________________________________                 */
006100060628     c     parcel        begsr
006200171018      *  __________________________________________________                 */
006300171018     c**** kar4          chain     fiar401l
006400171018     c******             if        %found(fiar401l)
006500171018     c****               movel     ar4not        dsbl4i
006600171018      ****
006700171018      *  se il parcel è di quelli vecchi a 12 caratteri inserisco nel campo */
006800171018      *  di ritorno anche il chkdigits perchè serve in bollettazione        */
006900171018     c****               if        %subst(§b4ipn:13:1) = *blank
007000171018     c****               eval      o24npr = %trim(§b4ipn) + %trim(§b4icd)
007100171018     c****               else
007200171018     c****               eval      o24npr = §b4ipn
007300171018     c****               endif
007400171018      ****
007500171018     c*******            endif
007600171018      *
007700171018      * aggancia il parcel nel nuovo file PND al posto del vecchio FIAR4
007800171018     c     kpnd          chain     fipnd01l
007900171018     c                   if        %found(fipnd01l)
008000171018     c                   eval      o24npr = pndIPN
008100171020      * se c'è il depot 7
008200171020     c                   if          pndIDP7 <> *blank
008300171020     c                   movel     pndIDP7       o24dep7
008400171020     c                   else
008500171020      * altrimenti il depot 4 dai primi caratteri del parcel
008600171020     c                   movel     pndIPN        o24dep4
008700171020     c                   endif
008800171018     c                   endif
008900171018
009000060628     c                   endsr
009100171020      *  ___________________________________________________                */
009200051202     c     *inzsr        begsr
009300171020      *  ___________________________________________________                */
009400051202     c     *entry        plist
009500060627     c                   parm                    fnlg24ds
009600051130      * originale
009700051130     c     kspe          klist
009800060627     c                   kfld                    i24aas
009900060627     c                   kfld                    i24lnp
010000060627     c                   kfld                    i24nrs
010100060627     c                   kfld                    i24nsp
010200171018      ******
010300171018     c**** kar4          klist
010400171018     c******             kfld                    kaas
010500171018     c******             kfld                    klnp
010600171018     c******             kfld                    knrs
010700171018     c******             kfld                    knsp
010800171018     c******             kfld                    ktrc
010900171018     c******             move      'I'           ktrc              1
011000171018      ******
011100171018     c     kpnd          klist
011200171018     c                   kfld                    kaas
011300171018     c                   kfld                    klnp
011400171018     c                   kfld                    knrs
011500171018     c                   kfld                    knsp
011600171020      *
011700171020      * imposta la UDATE
011800171020     c                   clear                   dateu             8 0
011900171020     c                   eval        dateu = *date
012000171020      *
012100060628     c                   endsr
012200171020      *  _____________________________________________________________      */
