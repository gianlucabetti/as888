000100000630      **************************************************************************
000200000630      *                                                                        *
000300000630      *   *----------------------------------------------------------------*   *
000400140430      *    PULIZIA FILES PER SEDE -   UNCMD00F - Contratti Comodato FILTRO     *
000500000630      *   *----------------------------------------------------------------*   *
000600000630      *                                                                        *
000700000630      **************************************************************************
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000801140430
000802140430     FTNSD72D   CF   E             WORKSTN
001000000630      *
001100000630      * DS PASSAGGIO PARAMETRI
001200000630      *
001201140430     D PARAM           DS            16
001202140430     D  datcdg                        8s 0
001203140430     D  datdus                        8s 0
001500000630     D KPJBA         E DS
001506131202      * - Parametri x Controllo profilo utenti
001507131202     d TIBS34ds      e ds                  inz
001508131202      * - Ds di riferimento al file esterno AZUTE00F
001509131202     d AZUTEds       e ds                  extname(AZUTE00F)
001510131202      * - Ds per dati organigramma
001511131202     d DDatiUte      e ds
001512140430     d dataiso         s               d   datfmt(*iso)
001513140430     d annopre         s              4  0
001600000630      **************************************************************************
001700000630      *                                                                        *
001800000630      *         O P E R A Z I O N I   I N I Z I A L I                          *
001900000630      *                                                                        *
002000000630      **************************************************************************
002100000630     C     *ENTRY        PLIST
002200000630     C                   PARM                    KPJBA
002201140430
002202140430     c                   clear                   param
002400000630      **************************************************************************
002500000630      *                                                                        *
002600000630      *         R O U T I N E   P R I N C I P A L E                            *
002700000630      *                                                                        *
002800000630      **************************************************************************
002900131202      * Operazioni Iniziali
003000131202     c                   exsr      RoutInz
003100131202      *
003200140430      * gestione formato video
003300140430     c                   exsr      GesD01
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
007103140430      *?Gestione formato video D01                                   ?*
007104140430      *---------------------------------------------------------------*
007105140430     c     Gesd01        BEGSR
007106140430      *
007107140430     c* DETERMINO LE DATE DI PULIZIA DA VISUALIZZARE E DA PASSARE AL BATCH:
007108140430     c* data di pulizia per gestione contratti annullati o chiusi
007109140430     c* --> 31/12 dell'anno precedente a quello corrente
007110140430     c     *year         sub       1             annopre
007111140430     c                   eval      datcdg=(annopre*10000)+1231
007112140430     c                   eval      v1cdg=(3112*10000)+annopre
007113140430     c* data di pulizia per data ultima spedizione fattura
007114140430     c* --> 1 anno rispetto alla data corrente
007115140430      /free
007116140430       dataiso=(%date()- %years(1));
007117140430       datdus=%dec(dataiso);
007118140430       v1dus = %dec(dataiso:*eur) ;
007119140430      /end-free
007120140430     c                   z-add     3             row
007121140430     c                   z-add     5             col
007122140430     c
007123140430     c                   dow       not *inkf and not *inkc
007124140430
007125140430     c                   exfmt     sd72d01
007126140430
007127140430     c* F6=Conferma --> sottometto azione di pulizia
007128140430     c                   if        *inkf
007129140430     c                   movel     param         kpjbu
007130140430     C                   MOVEL     'SD73'        KCOAZ
007131140430     c                   call      'BCH10'
007132140430     c                   parm                    kpjba
007133140430     c                   endif
007134140430
007135140430     c                   enddo
007136140430
007201140430     c                   ENDSR
007202140430
008700131202      *---------------------------------------------------------------*
008800131202      *?Operazioni Finali                                            ?*
008900131202      *---------------------------------------------------------------*
009000131202     c     RoutEnd       BEGSR
009100131202      *
009200131202     c                   eval      *inLR   = *on
009300131202     c                   return
009400131202      *
009500131202     c                   ENDSR
