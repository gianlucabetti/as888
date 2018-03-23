000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300060628      * ?__________________________________________________                 */
000400060627     ffnlbl01l  if   e           k disk
000500060627     ffiar401l  if   e           k disk
000600160826     fazorg01l  if   e           k disk
000700160826     fTIDP300F  o    e             disk
000800160826      * ?__________________________________________________                 */
000900160826     d fnlg28ds      e ds
001000160826      * ?__________________________________________________                 */
001100060627     D og143         e ds
001200060627     D dsbl4i        e ds
001300160826     d kaas            s                   like(i28aas)
001400160826     d klnp            s                   like(i28lnp)
001500160826     d knrs            s                   like(i28nrs)
001600160826     d knsp            s                   like(i28nsp)
001700060628      * ?__________________________________________________                 */
001800160826     c                   seton                                        lr
001900160826      *
002000060628      * ?se circuito DpD aggancia il fiar4 tipo rek'I' per reperire PARCEL  */
002100160826     c                   z-add     i28aas        kaas
002200160826     c                   z-add     i28lnp        klnp
002300160826     c                   z-add     i28nrs        knrs
002400160826     c                   z-add     i28nsp        knsp
002500060628      * ?__________________________________________________                 */
002600060628      * ?ricerca spedizione sui legami per agganciare bolla originale       */
002700060628     c     kspe          chain     fnlbl01l
002800060628     c                   if        %found(fnlbl01l)
002900060628      * ?verifica su azorg se circuito DpD                                  */
003000060628     c     lbllpo        chain     azorg01l
003100060628     c                   if        %found(azorg01l)
003200060628     C                   movel     orgde3        og143
003300060628     c                   z-add     LBLAAO        kaas
003400060628     c                   z-add     LBLLPO        klnp
003500060628     c                   z-add     LBLNRO        knrs
003600060628     c                   z-add     LBLNSO        knsp
003700060628      * ?se circuito DpD aggancia il fiar4 tipo rek'I' per reperire PARCEL  */
003800060628     C     §OgNTW        ifeq      'DPD'
003900060628     c                   exsr      parcel
004000060628     c                   endif
004100060628     c                   endif
004200060628     c                   endif
004300060628
004400160826     c                   RETURN
004500060628      * ?__________________________________________________                 */
004600060628     c     parcel        begsr
004700060628      * ?__________________________________________________                 */
004800060628     c     kar4          chain     fiar401l
004900060628     c                   if        %found(fiar401l)
005000060628     c                   movel     ar4not        dsbl4i
005100060628      * ?se il parcel è di quelli vecchi a 12 caratteri inserisco nel campo */
005200060628      * ?di ritorno anche il chkdigits perchè serve in bollettazione        */
005300060628     c                   if        %subst(§b4ipn:13:1) = *blank
005400060628     c                   eval      o24npr = %trim(§b4ipn) + %trim(§b4icd)
005500060628     c                   else
005600060628     c                   eval      o24npr = §b4ipn
005700060628     c                   endif
005800060628
005900060628     c                   endif
006000160826
006100060628     c                   endsr
006200060628      * ?___________________________________________________                */
006300051202     c     *inzsr        begsr
006400060628      * ?___________________________________________________                */
006500051202     c     *entry        plist
006600160826     c                   parm                    fnlg28ds
006700051130      * originale
006800051130     c     kspe          klist
006900160826     c                   kfld                    i28aas
007000160826     c                   kfld                    i28lnp
007100160826     c                   kfld                    i28nrs
007200160826     c                   kfld                    i28nsp
007300051202
007400060627     c     kar4          klist
007500060628     c                   kfld                    kaas
007600060628     c                   kfld                    klnp
007700060628     c                   kfld                    knrs
007800060628     c                   kfld                    knsp
007900060627     c                   kfld                    ktrc
008000060627     c                   move      'I'           ktrc              1
008100060628      *
008200060628     c                   endsr
008300060627      * ?_____________________________________________________________      */
