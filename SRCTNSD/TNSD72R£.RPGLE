000100000630      **************************************************************************
000200000630      *                                                                        *
000300000630      *   *----------------------------------------------------------------*   *
000400140430      *    PULIZIA FILES PER SEDE -   UNCMD00F - Contratti Comodato            *
000500000630      *   *----------------------------------------------------------------*   *
000600000630      *                                                                        *
000700000630      **************************************************************************
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000801140430     Funcmd01l  IF   E           K DISK
001000000630      *
001100000630      * DS PASSAGGIO PARAMETRI
001200000630      *
001201140430     D PARAM           DS            16
001202140430     D  datcdg                        8s 0
001203140430     D  datdus                        8s 0
001500000630     D KPJBA         E DS
001501140430     d TIBS69DS      E DS                  INZ
001502140430     d DS_cnaco      E DS                  extname(CNACO00F)
001503140430     d DS_cnind      E DS                  extname(CNIND00F)
001504140430     d DS_cnclp      E DS                  extname(CNCLP00F)
001505140430     d DS_fncls      E DS                  extname(FNCLS00F)
001506131202      * - Parametri x Controllo profilo utenti
001507131202     d TIBS34ds      e ds                  inz
001508131202      * - Ds di riferimento al file esterno AZUTE00F
001509131202     d AZUTEds       e ds                  extname(AZUTE00F)
001510131202      * - Ds per dati organigramma
001511131202     d DDatiUte      e ds
001512140430      * - Campi di comodo
001513140430     d savksc          s                   like(cmdksc)
001514140430     d okdus           s              1
001515140430     d datdus6         s              6  0
001516140430     d dataymd         s               d   datfmt(*ymd)
001600000630      **************************************************************************
001700000630      *                                                                        *
001800000630      *         O P E R A Z I O N I   I N I Z I A L I                          *
001900000630      *                                                                        *
002000000630      **************************************************************************
002100000630     C     *ENTRY        PLIST
002200000630     C                   PARM                    KPJBA
002300000630     C                   MOVEL     KPJBU         PARAM
002301140430      /free
002302140430       dataymd=%date(datdus:*iso);
002303140430       datdus6=%dec(dataymd);
002304140430      /end-free
002400000630      **************************************************************************
002500000630      *                                                                        *
002600000630      *         R O U T I N E   P R I N C I P A L E                            *
002700000630      *                                                                        *
002800000630      **************************************************************************
002900131202      * Operazioni Iniziali
003000131202     c                   exsr      RoutInz
003100131202      *
003200140430      * Lettura file UNCMD per la pulizia
003300140430     c                   exsr      Elabora
003301140429
003400131202      *
003500131202      * Operazioni Finali
003600131202     c                   exsr      RoutEnd
003700131202
003800131202      *?S U B R O U T I N E S?----------------------------------------*
003900131202
004000131202      *---------------------------------------------------------------*
004100131202      *?Operazioni Iniziali                                          ?*
004200131202      *---------------------------------------------------------------*
004300131202     c     RoutInz       BEGSR
004400131202      *
004500131202     c/exec sql
004600131202     c+     set  option  DYNUSRPRF = *OWNER,
004700131202     c+                  CLOSQLCSR = *ENDACTGRP
004800131202     c/end-exec
004900131202      *
005000131202      * Reperimento dati job
005100131202     c                   exsr      DatiJob
005200131202      *
005300131202     c                   ENDSR
005400131202
005500131202      *---------------------------------------------------------------*
005600131202      *?Reperimento Dati del job (Utente/Operativi)                  ?*
005700131202      *---------------------------------------------------------------*
005800131202     c     DatiJob       BEGSR
005900131202      *
006000131202     c     *dtaara       define    §azute        AZUTEds
006100131202     c     *dtaara       define    §datiute      dDatiUte
006200131202      *
006300131202     c                   in(E)     *dtaara
006400131202     c                   IF        %ERROR or RSUT = *blanks
006500131202     c                   clear                   TIBS34ds
006600131202     c                   call      'TIBS34R'
006700131202     c                   parm                    TIBS34ds
006800131202     c                   in        *dtaara
006900131202     c                   ENDIF
007000131202      *
007100131202     c                   ENDSR
007101140430
007102140430      *---------------------------------------------------------------*
007103140430      *?Lettura UNCMD per cancellazione                              ?*
007104140430      *---------------------------------------------------------------*
007105140430     c     Elabora       BEGSR
007106140430      *
007107140430     c                   clear                   okdus
007108140430     c                   clear                   savksc
007109140430     c     *loval        setll     uncmd01l
007110140430     c                   read      uncmd01l
007111140430     c                   dow       not %eof (uncmd01l)
007112140430     c* rottura di codice cliente
007113140430     c                   if        cmdksc<>savksc
007114140430     c                   if        savksc>0  and okdus='1'
007115140430     c                   exsr      Delete_UNCMD
007116140430     c                   endif
007117140430     c                   clear                   okdus
007118140430     c                   eval      savksc=cmdksc
007119140430     c                   endif
007120140430     c* letto record nè annullato nè chiuso oppure annullato/chiuso ma data gestione
007121140430     c* contratto > data pulizia
007122140430     c*  --> mi posiziono su nuovo cliente
007123140430     c                   if        (cmdcfg<>'A' and cmdcfg<>'C') or
007124140430     c                             cmdcdg>datcdg
007125140430     c                   clear                   okdus
007126140430     c     cmdksc        setgt     uncmd01l
007127140430     c                   read      uncmd01l
007128140430     c                   iter
007129140430     c                   endif
007130140430     c* record annullato o chiuso da almeno un anno:
007131140430     c* - verifico che cliente non spedisca da oltre un anno in base
007132140430     c*   alla data ultima spedizione fattura
007133140430     c                   if        okdus=*blanks
007134140430     c                   exsr      Sr_VerDus
007135140430     c                   if        okdus=*blanks
007136140430     c     cmdksc        setgt     uncmd01l
007137140430     c                   read      uncmd01l
007138140430     c                   iter
007139140430     c                   endif
007140140430     c                   endif
007141140430     c                   read      uncmd01l
007142140430     c                   enddo
007143140430     c* Eventuale pulizia dell'ultimo cliente letto
007144140430     c                   if        savksc>0  and okdus='1'
007145140430     c                   exsr      Delete_UNCMD
007146140430     c                   endif
007200131202
007201140430     c                   ENDSR
007202140430
007203140430      *---------------------------------------------------------------*
007204140430      *?Verifica data ultima spedizione fattura cliente              ?*
007205140430      *---------------------------------------------------------------*
007206140430     c     Sr_VerDus     BEGSR
007207140430     c                   clear                   tibs69ds
007208140430     c                   z-add     cmdksc        I69kcp
007209140430     c                   call      'TIBS69R'
007210140430     C                   parm                    tibs69DS
007211140430     C                   parm                    DS_cnaco
007212140430     C                   parm                    DS_cnind
007213140430     C                   parm                    DS_cnclp
007214140430     C                   parm                    DS_fncls
007215140430     c                   if        clpdus<=datdus6
007216140430     c                   eval      okdus='1'
007217140430     c                   endif
007218140430     c                   endsr
007300131202      *---------------------------------------------------------------*
007400131202      *?Cancellazione records                                        ?*
007500131202      *---------------------------------------------------------------*
007600140430     c     Delete_UNCMD  BEGSR
007700131202      *
007701140430      * Il test su CMDCFG lo metto per sicurezza ma non ce ne sarebbe
007702140430      * bisogno perchè se sono in questa routine ho già controllato
007703140430      * che il cliente non abbia contratti aperti
007800131202     c/exec sql
007900131202     c+     DELETE
008000140430     c+     from    UNCMD00F
008100140430     c+     where   cmdksc = :savksc
008200140430     c+       and   cmdcfg in ('A', 'C')
008300131202     c/end-exec
008400131202      *
008500131202     c                   ENDSR
008614140429
008700131202      *---------------------------------------------------------------*
008800131202      *?Operazioni Finali                                            ?*
008900131202      *---------------------------------------------------------------*
009000131202     c     RoutEnd       BEGSR
009100131202      *
009200131202     c                   eval      *inLR   = *on
009300131202     c                   return
009400131202      *
009500131202     c                   ENDSR
