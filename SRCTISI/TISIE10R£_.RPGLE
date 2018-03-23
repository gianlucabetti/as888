000100151019      * ?------------------------------------------------------------------ *
000200151019      * ?-  Riceve un DEPOT restituisce l'indirizzo originale preso dal     *
000300151019      * ?-  Cappario DPD.                                                   *
000400151019      * ?------------------------------------------------------------------ *
000500990504     H* ACTGRP=*CALLER
000600990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000700151019      * ?__________________________________________________                 *
000800150723     fdpcdp01l  if   e           k disk
000900151019      * ?__________________________________________________                 *
001000150723     d fnlg27ds      e ds
001100151019      * ?------------------------------------------------------------------ *
001200150723      * ?-  Parametri x controllo set di Caratteri con XCHKCHAR
001300151019      * ?------------------------------------------------------------------ *
001400150723     D TxtInOut        S           2048
001500150723     D ElencoChar      S            256
001600150723     D TipoElenco      S              1
001700150723     D CharSost        S              1
001800150723     D UpperCase       S              1
001900150723     D ChkNull         S              1
002000150723     D CharNull        S              1
002100150723     D Esito_check     S              1
002200150723     D  Dati           s           1500
002300150723      * ?------------------------------------------------------------------ */
002400150723      * ?controllo x CEDINTESA
002500150723     D SETA            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
002600150723     D                                     3456789 .,-()/=''+:?!"%&*;<>#@_°"£$')
002700150723     d bart_it         C                   CONST('@Bartolini.it;')
002800150723     d CED_Bart        C                   CONST('CEDAlert@Bartolini.it;')
002900150723     D   Digits        C                   '0123456789'
003000150723     D*------------------
003100150723      * ?set caratteri Maiuscoli/Minuscoli
003200150723     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
003300150723     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
003400060628      * ?__________________________________________________                 */
003500150723     c                   seton                                        lr
003600150723      * primi 4 caratteri del Parcel contengono il Depot di origine
003700150723      * del Parcel.
003800151019     c                   movel     i27DEP        Origin_Depot      4
003900150723      *
004000150723      *  Reperisce l'ultima versione del Cappario
004100150723     c     *hival        setll     dpcdp01l
004200150723     c                   readp     dpcdp01l
004300150723      *
004400150723     c                   if        not %EoF(dpcdp01l)
004500150723     c                   z-add     CDPVER        Versione          5 0
004600150723     c     K_CAPPARIO    CHAIN     dpcdp01l
004700150723     c                   if        %Found(dpcdp01l)
004800150723      **
004900150723     c                   eval      O27CDPPTC  = CDPPTC
005000150723      *
005100150723      *   Sposta il record a tracciato libero su un campo di lavoro       =
005200150723     c                   clear                   dati
005300150723     C     minu:maiu     XLATE     CDPNAM1       dati
005400150723     c                   exsr      Syntax
005500150723     c                   eval      O27CDPNAM1 = dati
005600150723      **
005700150723     c                   clear                   dati
005800150723     C     minu:maiu     XLATE     CDPNAM2       dati
005900150723     c                   exsr      Syntax
006000150723     c                   eval      O27CDPNAM2 = dati
006100150723      **
006200150723     c                   clear                   dati
006300150723     C     minu:maiu     XLATE     CDPADR1       dati
006400150723     c                   exsr      Syntax
006500150723     c                   eval      O27CDPADR1 = dati
006600150723      **
006700150723     c                   clear                   dati
006800150723     C     minu:maiu     XLATE     CDPADR2       dati
006900150723     c                   exsr      Syntax
007000150723     c                   eval      O27CDPADR2 = dati
007100150723      **
007200150723     c                   clear                   dati
007300150723     C     minu:maiu     XLATE     CDPCTY        dati
007400150723     c                   exsr      Syntax
007500150723     c                   eval      O27CDPCTY  = dati
007600150723      *
007700150723     c                   end
007800150723     c                   end
007900150723      *
008000150723     c                   RETURN
008100060628      * ?___________________________________________________                */
008200051202     c     *inzsr        begsr
008300060628      * ?___________________________________________________                */
008400051202     c     *entry        plist
008500150723     c                   parm                    fnlg27ds
008600150723      *   campi di output
008700150723     c                   clear                   O27CDPNAM1
008800150723     c                   clear                   O27CDPNAM2
008900150723     c                   clear                   O27CDPADR1
009000150723     c                   clear                   O27CDPADR2
009100150723     c                   clear                   O27CDPPTC
009200150723     c                   clear                   O27CDPCTY
009300150723     c                   clear                   O27CDPNAZ
009400060628      *
009500150723     c     K_CAPPARIO    klist
009600150723     C                   KFLD                    VERSIONE
009700150723     C                   KFLD                    ORIGIN_DEPOT
009800150723     C*------------------
009900060628     c                   endsr
010000060627      * ?_____________________________________________________________      */
010100150723      * *------------------------*
010200150723     C     Syntax        begSR
010300150723      * *------------------------*
010400150723      *
010500150723     c                   movel     dati          TxtInOut
010600150723     c                   eval      ElencoChar = SETA
010700150723     c                   eval      TipoElenco = *blank
010800150723     c                   eval      CharSost   = *blank
010900150723     c                   eval      UpperCase  = '1'
011000150723     c                   eval      ChkNull    = *blank
011100150723     c                   eval      CharNull   = *blank
011200150723     c                   eval      Esito_check= *blank
011300150723      *
011400150723     c                   call      'XCHKCHAR'
011500150723     c                   parm                    TxtInOut
011600150723     c                   parm                    ElencoChar
011700150723     c                   parm                    TipoElenco
011800150723     c                   parm                    CharSost
011900150723     c                   parm                    UpperCase
012000150723     c                   parm                    ChkNull
012100150723     c                   parm                    CharNull
012200150723     c                   parm                    Esito_check
012300150723      *
012400150723      *   se è stato modificato qualcosa nella riga di FLAT
012500150723     c                   if        Esito_check = '1'
012600150723     c                   movel     TxtInOut      dati
012700150723     c                   end
012800150723      *
012900150723     C                   ENDSR
013000150723      *-------------------------------------------------------------------------
