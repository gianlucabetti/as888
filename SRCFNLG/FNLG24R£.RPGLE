000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300060628      * ?__________________________________________________                 */
000400060627     ffnlbl01l  if   e           k disk
000500060627     ffiar401l  if   e           k disk
000600060627     fazorg01l  if   e           k disk
000700060627     d fnlg24ds      e ds
000800060627     D og143         e ds
000900060627     D dsbl4i        e ds
001000060628     d kaas            s                   like(i24aas)
001100060628     d klnp            s                   like(i24lnp)
001200060628     d knrs            s                   like(i24nrs)
001300060628     d knsp            s                   like(i24nsp)
001400060628      * ?__________________________________________________                 */
001500060628      * ?se circuito DpD aggancia il fiar4 tipo rek'I' per reperire PARCEL  */
001600060628     c                   z-add     i24aas        kaas
001700060628     c                   z-add     i24lnp        klnp
001800060628     c                   z-add     i24nrs        knrs
001900060628     c                   z-add     i24nsp        knsp
002000060628     c                   exsr      parcel
002100150714      *
002200150714     c                   if        o24npr = *blank
002300060628     c                   exsr      legami
002400060628     c                   endif
002500060628      *
002600000607     c                   seton                                        lr
002700060628      * ?__________________________________________________                 */
002800060628     c     legami        begsr
002900060628      * ?__________________________________________________                 */
003000060628      * ?ricerca spedizione sui legami per agganciare bolla originale       */
003100060628     c     kspe          chain     fnlbl01l
003200060628     c                   if        %found(fnlbl01l)
003300060628      * ?verifica su azorg se circuito DpD                                  */
003400060628     c     lbllpo        chain     azorg01l
003500060628     c                   if        %found(azorg01l)
003600060628     C                   movel     orgde3        og143
003700060628     c                   z-add     LBLAAO        kaas
003800060628     c                   z-add     LBLLPO        klnp
003900060628     c                   z-add     LBLNRO        knrs
004000060628     c                   z-add     LBLNSO        knsp
004100060628      * ?se circuito DpD aggancia il fiar4 tipo rek'I' per reperire PARCEL  */
004200060628     C     §OgNTW        ifeq      'DPD'
004300060628     c                   exsr      parcel
004400060628     c                   endif
004500060628     c                   endif
004600060628     c                   endif
004700060628
004800060628     c                   endsr
004900060628      * ?__________________________________________________                 */
005000060628     c     parcel        begsr
005100060628      * ?__________________________________________________                 */
005200060628     c     kar4          chain     fiar401l
005300060628     c                   if        %found(fiar401l)
005400060628     c                   movel     ar4not        dsbl4i
005500060628      * ?se il parcel è di quelli vecchi a 12 caratteri inserisco nel campo */
005600060628      * ?di ritorno anche il chkdigits perchè serve in bollettazione        */
005700060628     c                   if        %subst(§b4ipn:13:1) = *blank
005800060628     c                   eval      o24npr = %trim(§b4ipn) + %trim(§b4icd)
005900060628     c                   else
006000060628     c                   eval      o24npr = §b4ipn
006100060628     c                   endif
006200060628
006300060628     c                   endif
006400060628     c                   endsr
006500060628      * ?___________________________________________________                */
006600051202     c     *inzsr        begsr
006700060628      * ?___________________________________________________                */
006800051202     c     *entry        plist
006900060627     c                   parm                    fnlg24ds
007000051130      * originale
007100051130     c     kspe          klist
007200060627     c                   kfld                    i24aas
007300060627     c                   kfld                    i24lnp
007400060627     c                   kfld                    i24nrs
007500060627     c                   kfld                    i24nsp
007600051202
007700060627     c     kar4          klist
007800060628     c                   kfld                    kaas
007900060628     c                   kfld                    klnp
008000060628     c                   kfld                    knrs
008100060628     c                   kfld                    knsp
008200060627     c                   kfld                    ktrc
008300060627     c                   move      'I'           ktrc              1
008400060628      *
008500060628     c                   endsr
008600060627      * ?_____________________________________________________________      */
