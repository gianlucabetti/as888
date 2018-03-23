000100170804       //--------------------------------------------------------------
000200170804       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000300170804       //--------------------------------------------------------------
000400170804
000500170804     /*PRM  dbgview(*source)
000600170804     /*CMD  ovrdbf file(TABELX0F) tofile(TABEL00F) +
000700170804     /*CMD         ovrscope(*calllvl)
000800170804     /*END  dltovr file(TABELX0F) lvl(*)
000900170804     /*END
001000170804
001100170804       //--------------------------------------------------------------
001200170804
001300891030     H DFTNAME(CN0500) DATEDIT(*YMD)
001400170804     h option(*nodebugio)
001500110811
001600110811      *?  ATTENZIONE!!  ?
001700110811      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
001800110811      *?    'Interrogazione abilitazioni clienti'              ?
001900110811      *?    In caso di aggiunta/modifica campi alla tabella    ?
002000110811      *?    verificare se sono validi per la visualizzazione   ?
002100110811      *?    da TA61                                            ?
002200110811
002300891030     F*----------------------* USO DEGLI INDICATORI *----------------*
002400931209     F*  KC        FINE                                              *
002500940228     F*  KE        RIPRISTINO                                        *
002600940228     F*  KF        CONFERMA                                          *
002700931209     F*  KL        RITORNO                                           *
002800931209     F*  KQ        ANNULLAMENTO                                      *
002900891030     F*--------------------------------------------------------------*
003000940929     FTRTB71D   CF   E             WORKSTN
003100940929     F*----------
003200940929     FTABEL00F  UF A E           K DISK
003300170120     fTABELx0F  Uf A e           k disk    rename(tabel:tabelx) prefix(x)
003400170120     F*----------
003500120302     ftntbe01l  if   e           k disk
003600170807
003700170807       //--------------------------------------------------------------
003800170807       //?Definizione costanti.                                        ?
003900170807       //--------------------------------------------------------------
004000170807
004100170807       // -?Costante per controllo "caratteri solo numerici"?
004200170807     d c_Digits        c                   const('0123456789')
004300170807
004400940418     D*--------------------------------------------------------------*
004500940418     D*   SCHIERE                                                    *
004600940701     D*--------------------------------------------------------------*
004700940929     D*  Caricamento capoconto clienti
004800940929     D*--------------------------------------------------------------*
004900120302     D ERR             S             70    DIM(9) CTDATA PERRCD(1)
005000940929     D COST            S             16    DIM(3) CTDATA PERRCD(1)
005100940928     D*--------------------------------------------------------------*
005200940929     D VFLP            S              1    DIM(4)
005300940929     D DPAR            S             31    DIM(4) CTDATA PERRCD(1)
005400940929     D KPAR            S              1    DIM(4) CTDATA PERRCD(4)
005500940929     D PGM             S             10    DIM(4) CTDATA PERRCD(1)
005600940929     D*--------------------------------------------------------------*
005700170807     ***D VCP             S              2    DIM(30)
005800170807     d VCP_ds          ds            60    inz
005900170807     D   VCP                          2    dim(30)
006000940929     D IN1             S              2  0 DIM(30) CTDATA PERRCD(30)
006100940929     D*--------------------------------------------------------------*
006200090225     D C3K             S              7    DIM(9999)
006300090225     D D3K             S             89    DIM(9999)
006400090224     D*--------------------------------------------------------------*
006500110210     D C7Q             S              2    DIM(9999)
006600110210     D C7P             S              2    DIM(9999)
006700110210     D C7R             S              2    DIM(9999)
006800110210     D C7S             S              2    DIM(9999)
006900110210     D C7Rvag          S              1    DIM(9999)
007000120925     D C7Rbam          S              2    DIM(999)
007100940929     D*--------------------------------------------------------------*
007200170804     ***D CPA             S              2    DIM(50)
007300170804     d CPA_ds          ds           100    inz
007400170804     d   CPA                          2    dim(50)
007500940929     D COMO            S            100    DIM(4)
007600940928     D*--------------------------------------------------------------*
007700940928     D*  DS                                                          *
007800940928     D*--------------------------------------------------------------*
007900940930     D DSTB71        E DS                  EXTNAME(DS7PQRS)
008000940929     D*
008100170120     d Tabelds       e ds                  extname(TABEL00F) inz
008200170120     d xTabelds      e ds                  extname(TABELX0F) inz
008300170120     d                                     prefix(x)
008400170120      *-
008500940929     D DS7U          E DS
008600090224     D DS7R          E DS
008700090224     D DS3K          E DS
008800170804
008900170804       // -?Tabella OSR
009000170804     d dOSR          e ds
009100900608      *
009200891030     D KPJBA         E DS
009300911118      *
009400971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
009500170804     ***D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
009600170804     ***D  TCU                  398    697
009700170804     ***D                                     DIM(50)
009800170804     ***D  KCU                  698    847P 0
009900170804     ***D                                     DIM(50)
010000170804     ***D                                     PACKEVEN
010100980317     D TIBS69        E DS                  EXTNAME(TIBS69DS)
010200980317     D CNACO         E DS                  EXTNAME(CNACO00F)
010300980317     D CNIND         E DS                  EXTNAME(CNIND00F)
010400980317     D CNCLP         E DS                  EXTNAME(CNCLP00F)
010500980317     D FNCLS         E DS                  EXTNAME(FNCLS00F)
010600170804     ***D TCUDS           DS
010700170804     ***D  F3                     3      3
010800170804     ***D  F4                     4      4
010900170804     ***D  F56                    5      6
011000960212     D                 DS
011100960212     D  WNUM                   1      7  0
011200960212     D  WCOD                   8      8
011300960212     D  WKEY                   1      8
011400170804      *
011500170804      * -?Parametri aggiuntivi (ricevuti da TNTB30R - Gest. tab."OSR")?
011600170804     d TNTB30ds        ds                  qualified
011700170807     d   §OSRopz                      1
011800170804     d   §OSRcli                      7s 0
011900170804     d   §OSRtpa                      1
012000170804     d   §OSRcpa                      2
012100170804     d   §OSRerr                      1
012200170804     d   §OSRmsg                     78
012300940228      *
012400940228     D                SDS
012500940927     D  V1CPGM                 1     10
012600170804      *--------------------------------------------------------------*
012700170804      *  VARIABLI                                                    *
012800170804      *--------------------------------------------------------------*
012900170804     d Ktbecod         s                   like(TBECOD)
013000170804     d                                     inz('OSR')
013100170804     d wokOSR          s               n   inz(*off)
013200170807     d $FoundPartic    s               n   inz
013300170807     d wTipoPartic     s             16a   inz
013400170804
013500170804       //--------------------------------------------------------------
013600170804       //?Definizione aree dati.                                       ?
013700170804       //--------------------------------------------------------------
013800170804
013900170804       // -?Dati utente?
014000170804     d §AzUte        e ds                  extname(AZUTE00F)
014100170804     d                                     dtaara
014200170804     d §DatiUte      e ds                  extname(dDatiUte)
014300170804     d                                     dtaara
014400170804
014500170804       //--------------------------------------------------------------
014600170804       //?Definizione prototipi procedure.                             ?
014700170804       //--------------------------------------------------------------
014800170804
014900170804       // -?Reperimento dati utente?
015000170804     d TIBS34ds      e ds                  inz
015100170804      /copy gaitrasrc/srcProtoPR,TIBS34R
015200170804
015300891030     I*--------------------------------------------------------------*
015400891030     I*                        I N P U T                             *
015500891030     I*--------------------------------------------------------------*
015600940929     ITB71D01
015700940929     I              V1CCAS                      VFLP(1)
015800940929     I              V1CGIA                      VFLP(2)
015900940929     I              V1CCON                      VFLP(3)
016000940929     I              V1CARR                      VFLP(4)
016100940929     ITB71D02
016200940929     I              V2CC01                      VCP(1)
016300940929     I              V2CC02                      VCP(2)
016400940929     I              V2CC03                      VCP(3)
016500940929     I              V2CC04                      VCP(4)
016600940929     I              V2CC05                      VCP(5)
016700940929     I              V2CC06                      VCP(6)
016800940929     I              V2CC07                      VCP(7)
016900940929     I              V2CC08                      VCP(8)
017000940929     I              V2CC09                      VCP(9)
017100940929     I              V2CC10                      VCP(10)
017200940929     I              V2CC11                      VCP(11)
017300940929     I              V2CC12                      VCP(12)
017400940929     I              V2CC13                      VCP(13)
017500940929     I              V2CC14                      VCP(14)
017600940929     I              V2CC15                      VCP(15)
017700940929     I              V2CC16                      VCP(16)
017800940929     I              V2CC17                      VCP(17)
017900940929     I              V2CC18                      VCP(18)
018000940929     I              V2CC19                      VCP(19)
018100940929     I              V2CC20                      VCP(20)
018200940929     I              V2CC21                      VCP(21)
018300940929     I              V2CC22                      VCP(22)
018400940929     I              V2CC23                      VCP(23)
018500940929     I              V2CC24                      VCP(24)
018600940929     I              V2CC25                      VCP(25)
018700940929     I              V2CC26                      VCP(26)
018800940929     I              V2CC27                      VCP(27)
018900940929     I              V2CC28                      VCP(28)
019000940929     I              V2CC29                      VCP(29)
019100940929     I              V2CC30                      VCP(30)
019200931209     C*--------------------------------------------------------------*
019300931209     C*    C I C L O    P R I N C I P A L E                          *
019400931209     C*--------------------------------------------------------------*
019500940928     C                   EXSR      INZ01
019600940928     C*  Loop gestione videate
019700940927     C     WFINE         DOWNE     'S'
019800940929     C     WTPVID        CASEQ     '1'           GESD01
019900931209     C                   END
020000931209     C                   END
020100931209     C* Fine programma
020200931209     C     FINE          TAG
020300980317     C                   CLEAR                   TIBS69
020400980317     C                   MOVEL     'C'           I69TLA
020500980317     C                   CALL      'TIBS69R'
020600980317     C                   PARM                    TIBS69
020700980317     C                   PARM                    CNACO
020800980317     C                   PARM                    CNIND
020900980317     C                   PARM                    CNCLP
021000980317     C                   PARM                    FNCLS
021100980317     C*
021200931209     C                   SETON                                        LR
021300940929     C*---------------------------------------------------*
021400940929     C*  Gestione prima videata
021500940929     C*---------------------------------------------------*
021600940929     C     GESD01        BEGSR
021700940929     C*  Emissione videata
021800940929     C                   MOVEL     *BLANKS       V1DFUN
021900940929     C                   WRITE     TB71T01
022000940929     C                   EXFMT     TB71D01
022100940929     C                   SETOFF                                       404142
022200940929     C                   SETOFF                                       434428
022300940929     C                   MOVEL     *BLANKS       $MSG
022400940929     C*---------------------------------------------------*
022500940929     C*  Fine elaborazione
022600940929     C     *INKC         IFEQ      '1'
022700940929     C                   MOVEL     'S'           WFINE
022800940929     C                   GOTO      FINVD1
022900940929     C                   END
023000940929     C*---------------------------------------------------*
023100940929     C*  Richiamo Ricerca alfabetica
023200940929     C     *INKB         IFEQ      '1'
023300940929     C                   MOVEL     V1DCLI        ACORAG
023400170804     ***C                   Z-ADD     CLIITA        SAVKCI            4 0
023500170804     C                   z-add     DUTkci        SAVKCI            4 0
023600940929     C                   MOVE      V1CCLI        SAVKSC            7 0
023700940929     C                   CALL      'XALFA1'
023800940929     C                   PARM                    V1CRSU
023900940929     C                   PARM                    KKUT
024000940929     C                   PARM                    ACORAG
024100940929     C                   PARM                    SAVKCI
024200940929     C                   PARM                    SAVKSC
024300940929     C                   PARM                    PARSTA            1 0
024400961017     C                   PARM                    PARFLR           90
024500940929     C     PARSTA        IFNE      -1
024600940929     C                   MOVE      SAVKSC        V1CCLI
024700940929     C                   MOVEL     ACORAG        V1DCLI
024800940929     C                   END
024900940929     C                   GOTO      FINVD1
025000940929     C                   END
025100940929     C*---------------------------------------------------*
025200940929     C* Controlli
025300940929     C                   EXSR      CTR01
025400940929     C* Se non ci sono errori emetto seconda videata
025500940929     C     *IN28         IFEQ      '0'
025600940929     C     *IN90         ANDEQ     '0'
025700940929     C                   EXSR      RICD02
025800940929     C                   END
025900940929     C*
026000940929     C     FINVD1        ENDSR
026100940929     C*---------------------------------------------------*
026200940929     C*  Gestione Seconda videata
026300940929     C*---------------------------------------------------*
026400940929     C     GESD02        BEGSR
026500940929     C*  Emissione videata
026600940929     C                   WRITE     TB71T01
026700940929     C                   EXFMT     TB71D02
026800940929     C                   SETOFF                                       404142
026900940929     C                   SETOFF                                       434445
027000940929     C                   SETOFF                                       464748
027100940929     C                   SETOFF                                       495051
027200940929     C                   SETOFF                                       525354
027300940929     C                   SETOFF                                       555657
027400940929     C                   SETOFF                                       585960
027500940929     C                   SETOFF                                       616263
027600940929     C                   SETOFF                                       646566
027700940929     C                   SETOFF                                       676869
027800060307     C                   SETOFF                                       28
027900940929     C                   MOVEL     *BLANKS       $MSG
028000940929     C*  Fine elaborazione
028100940929     C     *INKC         IFEQ      '1'
028200940929     C                   MOVEL     'S'           WFINE             1
028300940929     C                   GOTO      FINVD2
028400940929     C                   END
028500940929     C*  Ritorno prima videata
028600940929     C     *INKL         IFEQ      '1'
028700160901     c                   except    vuoto
028800940929     C                   GOTO      FINVD2
028900940929     C                   END
029000940929     C*  Annullamento
029100940929     C     *INKQ         IFEQ      '1'
029200940929     C                   EXSR      ANNUL
029300170120     C                   EXSR      AGGIORx
029400940929     C                   MOVEL     'S'           WFINE
029500940929     C                   GOTO      FINVD2
029600940929     C                   END
029700940929     C*  controlli
029800940929     C                   EXSR      CTR02
029900960209     C   28              MOVEL     '0'           *INKF
030000960209     C   28              GOTO      FINVD2
030100940929     C*  Ripristino
030200940929     C     *INKE         IFEQ      '1'
030300940929     C                   EXSR      RIPRIS
030400170120     C                   EXSR      AGGIORx
030500940929     C                   MOVEL     'S'           WFINE
030600120925     c* se c'e' una particolarità di restituizione doc mittente
030700120925     c* chiamo  tabella CLI
030800120925     c                   if        chiamacli='S'
030900120925     c                   clear                   kpjbu
031000120926     c                   movel     v2ccli        kpjbu
031100120925     c                   call      'TNTB53R'
031200120925     c                   parm                    kpjba
031300120925     c                   endif
031400120925     c
031500940929     C                   GOTO      FINVD2
031600940929     C                   END
031700940929     C*  Conferma
031800940929     C     *INKF         IFEQ      '1'
031900940929     C                   EXSR      AGGIOR
032000170120     C                   EXSR      AGGIORx
032100120925     c
032200120925     c* se c'e' una particolarità di restituizione doc mittente
032300120925     c* chiamo  tabella CLI solo la prima volta o quando la tolgo la partic
032400120925     c                   if        (chiamacli='S' and giadoc=' ') or
032500120925     c                             (chiamacli=' ' and giadoc='S')
032600120925     c                   clear                   kpjbu
032700120926     c                   movel     v2ccli        kpjbu
032800120925     c                   call      'TNTB53R'
032900120925     c                   parm                    kpjba
033000120925     c                   endif
033100120925     c
033200940929     C                   GOTO      FINVD2
033300940929     C                   END
033400940929     C*----------------------------------------------------*
033500940929     C     FINVD2        ENDSR
033600940929     C*----------------------------------------------------*
033700940929     C*  INZ01   Gestione inizializzazione prima videata   *
033800940929     C*----------------------------------------------------*
033900940929     C     INZ01         BEGSR
034000940929     C*  Inizializzo prima videata
034100940929     C                   MOVEL     '1'           WTPVID            1
034200940929     C                   MOVEL     *BLANKS       V1CCLI
034300940929     C                   MOVEL     *BLANKS       V1DCLI
034400940929     C                   MOVEA     *BLANKS       VFLP
034500940929     C                   MOVEL     *BLANKS       $MSG
034600940929     C                   SETOFF                                       404142
034700940929     C                   SETOFF                                       434445
034800170804      *
034900170804      * -?SE richiamato dal pgm TNTB30R (Gest.Tab."OSR")?
035000170804      *  ?è ricevuto come parametro il codice cliente?
035100170807      *  ?e quel cliente va (dis)abilitato automaticamente alla?
035200170807      *    ?particolarità consegna "RC" - a seconda dell'opzione?
035300170807      *    ?ricevuta?
035400170804     c                   if        %parms()         > 1      and
035500170804     c                             TNTB30ds.§OSRcli > *zero  and
035600170804     c                             TNTB30ds.§OSRcpa > *blank
035700170804     c                   exsr      sr_Abi_PaCo_RC
035800170804     c                   eval      wFine = 'S'
035900170804     c                   endif
036000940929     C*
036100940929     C                   ENDSR
036200170804
036300170804      /free
036400170804
036500170804       //--------------------------------------------------------------
036600170807       //?(Dis)Abilitazione automatica del singolo cliente alla        ?
036700170804       //?  Particolarità Consegna "RC".                               ?
036800170807       //?· §OSRopz = *blank => manutenzione/inserimento               ?
036900170807       //?· §OSRopz = "4"    => annullamento                           ?
037000170807       //?· §OSRopz = "7"    => ripristino                             ?
037100170804       //--------------------------------------------------------------
037200170807       //?Errori previsti:                                             ?
037300170807|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7....+...
037400170807Operaz*one NON eseguibile in tab. "7U" per il cliente 1234567
037500170807Codice*cliente obbligatorio per tab. "7U"
037600170807Elimin*te Particolarità *...+....1....+. già annullate per il cliente 1234567
037700170807Già pr*senti ANCHE altre Particolarità *...+....1....+. per il cliente 1234567
037800170807Partic*larità *...+....1....+. "xx" non inseribile per il cliente 1234567
037900170807       //--------------------------------------------------------------
038000170804       BEGSR  sr_Abi_PaCo_RC;
038100170804
038200170804         clear  TNTB30ds.§OSRerr;
038300170804         clear  TNTB30ds.§OSRmsg;
038400170807
038500170807         // -?Controllo parametri ricevuti?
038600170807         Select;
038700170807           When  TNTB30ds.§OSRopz <> *blank  and
038800170807                 TNTB30ds.§OSRopz <> '4'     and
038900170807                 TNTB30ds.§OSRopz <> '7';
039000170807             TNTB30ds.§OSRerr = *on;
039100170807             TNTB30ds.§OSRmsg = 'Operazione NON eseguibile in tab. +
039200170807                                 "7U" per il cliente ' +
039300170807                                %editc( TNTB30ds.§OSRcli : 'X' );
039400170807             leavesr;
039500170807           When  %check( c_Digits : %subst( TNTB30ds : 2 :
039600170807                         %len( TNTB30ds.§OSRcli ) ) ) > *zero;
039700170807             TNTB30ds.§OSRerr = *on;
039800170807             TNTB30ds.§OSRmsg = 'Codice cliente obbligatorio per +
039900170807                                 tab. "7U"';
040000170807             leavesr;
040100170807           When  TNTB30ds.§OSRcli <= *zero;
040200170807             TNTB30ds.§OSRerr = *on;
040300170807             TNTB30ds.§OSRmsg = 'Codice cliente obbligatorio per +
040400170807                                 tab. "7U"';
040500170807             leavesr;
040600170807         EndSl;
040700170804
040800170804         // -?Inizializzazione 1ª videata (non emessa)?
040900170804         V1Ccli = %editc( TNTB30ds.§OSRcli : 'X' );
041000170804
041100170804         clear  x;
041200170804         x = %lookup( TNTB30ds.§OSRtpa : KPAR );
041300170804
041400170804         if  x = *zero;
041500170804           leavesr;
041600170804         endif;
041700170804
041800170804         VFLP(x) = '1';
041900170807
042000170807         xx = %scan( '''' : dPar(x) );
042100170807         if  xx > *zero;
042200170807           wTipoPartic = %trim( %subst( dPar(x) : xx + 1 ) );
042300170807         endif;
042400170804
042500170804         // -?Controllo dati ricevuti?
042600170804         exsr  Ctr01;
042700170804
042800170804         if  *in28;
042900170804           TNTB30ds.§OSRerr = *on;
043000170804           TNTB30ds.§OSRmsg = $Msg;
043100170804           leavesr;
043200170804         endif;
043300170804
043400170804         // -?Inizializzazione 2ª videata (non emessa)?
043500170804         V2Ccli = %int( V1Ccli );
043600170804         V2Dcli = V1Dcli;
043700170804         wNUM   = %int( V1Ccli );
043800170804         KKEY   = %editc( wNUM : 'X' ) + kPAR(x);
043900170804         V2Dpar = dPAR(x);
044000170804         CPA_ds = Como(x);         // -?MOVEA !?
044100170804         PgmPar = Pgm(x);
044200170804         exsr  Inz02;
044300170804
044400170807         // -?SE record annullato: si considera SENZA particolarità?
044500170807         If  TBLflg <> *blank;
044600170807
044700170807           // -?Restituzione messaggio di avvertimento per segnalare?
044800170807           //  ?che per il cliente erano presenti altre abilitazioni?
044900170807           //  ?ANNULLATE (che a breve verranno cancellate)?
045000170807           if  TNTB30ds.§OSRerr = *blank;
045100170807
045200170807             For  xx = 1  To  %elem( VCP );
045300170807
045400170807               if  VCP( xx ) <> *blank  and
045500170807                   VCP( xx ) <> TNTB30ds.§OSRcpa;
045600170807                 TNTB30ds.§OSRerr = 'A';
045700170807                 TNTB30ds.§OSRmsg = 'Eliminate Particolarità ' +
045800170807                                     %trim(wTipoPartic) + ' +
045900170807                                     già annullate per il cliente ' +
046000170807                                     %editc( TNTB30ds.§OSRcli : 'X' );
046100170807                 leave;
046200170807               endif;
046300170807
046400170807             EndFor;
046500170807
046600170807           endif;
046700170807
046800170804           clear  VCP;
046900170807
047000170807         EndIf;
047100170804
047200170807         // -?§OSRopz = *blank => manutenzione/inserimento:?
047300170807         // ·?Impostazione Particolarità Consegna "RC" (ricevuta in?
047400170804         //  ?§OSRcpa) in tab. "7U" - SE mancante!?
047500170807         // -?§OSRopz = "4"    => annullamento:?
047600170807         // ·?Annullamento Particolarità Consegna "RC" (ricevuta in?
047700170807         //  ?§OSRcpa) in tab. "7U" - SE reperita!?
047800170807         // -?§OSRopz = "7"    => ripristino:?
047900170807         // ·?Ripristino   Particolarità Consegna "RC" (ricevuta in?
048000170807         //  ?§OSRcpa) in tab. "7U" - SE reperita annullata!?
048100170807         // ·?Impostazione Particolarità Consegna "RC" (ricevuta in?
048200170807         //  ?§OSRcpa) in tab. "7U" - SE mancante!?
048300170807         if  ( TNTB30ds.§OSRopz                  = *blank  and
048400170807               %lookup( TNTB30ds.§OSRcpa : VCP ) > *zero )  OR
048500170807             ( TNTB30ds.§OSRopz                  = '4'     and
048600170807               %lookup( TNTB30ds.§OSRcpa : VCP ) = *zero )  OR
048700170807             ( TNTB30ds.§OSRopz                  = '7'     and
048800170807               %lookup( TNTB30ds.§OSRcpa : VCP ) > *zero );
048900170804           leavesr;
049000170804         endif;
049100170804
049200170807         // -?Inserimento/Ripristino Particolarità?
049300170807         If  TNTB30ds.§OSRopz = *blank  or
049400170807             TNTB30ds.§OSRopz = '7';
049500170807
049600170807           // -?Restituzione messaggio di avvertimento per segnalare?
049700170807           //  ?che per il cliente sono già presenti altre abilitazioni?
049800170807           For  xx = 1  To  %elem( VCP );
049900170807
050000170807             if  VCP( xx ) <> *blank  and
050100170807                 VCP( xx ) <> TNTB30ds.§OSRcpa;
050200170807               TNTB30ds.§OSRerr = 'A';
050300170807               TNTB30ds.§OSRmsg = 'Già presenti ANCHE altre +
050400170807                                   Particolarità ' + %trim(wTipoPartic) +
050500170807                                 ' per il cliente ' +
050600170807                                   %editc( TNTB30ds.§OSRcli : 'X' );
050700170807               leave;
050800170807             endif;
050900170807
051000170807           EndFor;
051100170807
051200170807           xx = %lookup( *blank : VCP );
051300170807
051400170807           if  xx = *zero;
051500170807             TNTB30ds.§OSRerr = *on;
051600170807             TNTB30ds.§OSRmsg = 'Particolarità ' + %trim(wTipoPartic) +
051700170807                                ' "' + TNTB30ds.§OSRcpa +
051800170807                                '" non inseribile per il cliente ' +
051900170807                                %editc( TNTB30ds.§OSRcli : 'X' );
052000170807             leavesr;
052100170807           endif;
052200170807
052300170807           VCP(xx)   = TNTB30ds.§OSRcpa;
052400170807
052500170807         // -?Annullamento Particolarità?
052600170807         ElseIf  TNTB30ds.§OSRopz = '4';
052700170807
052800170807           xx = %lookup( TNTB30ds.§OSRcpa : VCP );
052900170807
053000170807           if  xx = *zero;
053100170807             leavesr;
053200170807           endif;
053300170807
053400170807           clear  VCP(xx);
053500170807
053600170807         Endif;
053700170804
053800170804         // -?Controllo dati impostati automaticamente?
053900170807         If  TNTB30ds.§OSRopz = *blank  or
054000170807             TNTB30ds.§OSRopz = '7';
054100170807
054200170807           exsr  Ctr02;
054300170807
054400170807           if  *in28;
054500170807             TNTB30ds.§OSRerr = *on;
054600170807             TNTB30ds.§OSRmsg = $Msg;
054700170807             leavesr;
054800170807           endif;
054900170807
055000170807         EndIf;
055100170807
055200170807         // -?Richiesto Annullamento singola Particolarità?
055300170807         //  ?=> verifica esistenza altre particolarità?
055400170807         //  ?(per NON annullare il record con tutte le?
055500170807         //  ?Particolaritò, ma togliere solo quella da?
055600170807         //  ?annullare)?
055700170807         If  TNTB30ds.§OSRopz = '4';
055800170807
055900170807           $FoundPartic = *off;
056000170807
056100170807           For  xx = 1  To  %elem( VCP );
056200170807             if  VCP( xx ) <> *blank;
056300170807               $FoundPartic = *on;
056400170807               leave;
056500170807             endif;
056600170807           EndFor;
056700170807
056800170807         EndIf;
056900170807
057000170807         // -?Scrittura/Aggiornamento/Ripristino Particolarità?
057100170807         If  TNTB30ds.§OSRopz <> '4'  or
057200170807             VCP_ds           <> *blank;
057300170807           exsr  AGGIOR;
057400170807         // -?Annullamento Particolarità?
057500170807         ElseIf  TNTB30ds.§OSRopz = '4'  and
057600170807                 $FoundPartic     = *off;
057700170807           exsr  ANNUL;
057800170807         EndIf;
057900170807
058000170807         exsr  AGGIORx;
058100170804
058200170804         // -?SE c'e' una particolarità di restituizione doc mittente?
058300170804         //  ?chiamo  tabella CLI  solo la prima volta?
058400170804         if  ( chiamacli = 'S'  and  giadoc = ' ' )  or
058500170804             ( chiamacli = ' '  and  giadoc = 'S' );
058600170804           clear  kpjbu;
058700170804           kpjbu = %editc( V2Ccli : 'X' );
058800170804      /end-free
058900170804     c                   call      'TNTB53R'
059000170804     c                   parm                    kpjba
059100170804      /free
059200170804           endif;
059300170804
059400170804       ENDSR;
059500170804
059600170804      /end-free
059700170804
059800940929     C*----------------------------------------------------*
059900940929     C*  RICD02  Richiamo gestione 2 videata               *
060000940929     C*----------------------------------------------------*
060100940929     C     RICD02        BEGSR
060200940929     C                   MOVE      V1CCLI        V2CCLI
060300940929     C                   MOVE      V1DCLI        V2DCLI
060400960212     C                   MOVEL     V1CCLI        WNUM
060500960212     C                   MOVEL     WNUM          KKEY
060600940929     C* Controllo particolarità selezionate
060700940929     C                   Z-ADD     0             X
060800940929     C     X             DOWLT     4
060900940929     C     *INKC         ANDNE     '1'
061000940929     C                   ADD       1             X
061100940929     C     VFLP(X)       IFEQ      '1'
061200940929     C                   MOVE      KPAR(X)       KKEY
061300940929     C                   MOVEL     DPAR(X)       V2DPAR
061400940929     C                   MOVEA     COMO(X)       CPA
061500940929     C                   MOVEL     PGM(X)        PGMPAR           10
061600940929     C                   EXSR      INZ02
061700940929     C     WFINE         DOWNE     'S'
061800940929     C     *INKL         ANDEQ     '0'
061900940930     C     *INKF         ANDEQ     '0'
062000940929     C                   EXSR      GESD02
062100940929     C                   END
062200940929     C                   END
062300941003     C                   MOVE      '0'           *INKL
062400941003     C                   MOVE      '0'           *INKF
062500940929     C                   END
062600940929     C*
062700940929     C     *INKC         IFNE      '1'
062800940929     C                   EXSR      INZ01
062900940929     C                   END
063000940929     C*
063100940929     C                   ENDSR
063200940929     C*----------------------------------------------------*
063300940929     C*  INZ02   Gestione inizializz. seconda videata      *
063400940929     C*----------------------------------------------------*
063500940929     C     INZ02         BEGSR
063600120925     c                   clear                   giadoc            1
063700940929     C*
063800960228     C                   SETOFF                                       03
063900960228     C     X             IFEQ      3
064000960228     C                   SETON                                        03
064100960228     C                   END
064200040806     C                   SETOFF                                       02
064300040806     C     X             IFEQ      2
064400040806     C                   SETON                                        02
064500040806     C                   END
064600940929     C                   MOVE      '7U'          KCOD
064700940929     C     KTAB2         CHAIN     TABEL00F                           31
064800940929     C     *IN31         IFEQ      '0'
064900940929     C                   MOVEL     TBLUNI        DS7U
065000940929     C     TBLFLG        IFNE      ' '
065100940929     C                   SETON                                        92
065200940929     C                   MOVEL     COST(3)       V1DFUN
065300940929     C                   ELSE
065400940929     C                   SETON                                        93
065500940929     C                   MOVEL     COST(2)       V1DFUN
065600940929     C                   END
065700940929     C                   ELSE
065800940929     C                   MOVEL     COST(1)       V1DFUN
065900940929     C                   CLEAR                   DS7U
066000940929     C                   END
066100940929     C*
066200960228     C                   MOVEL     §7USTB        V2CSTB
066300040806     C                   MOVEL     §7USRE        V2CSRS
066400060307     C                   MOVEL     §7Uldcr       V2Cldcr
066500940929     C                   MOVEA     §7UCPA        VCP
066600960223     C                   MOVEL     ' '           V2CTRA
066700940930     C     TBLFTT        IFEQ      '1'
066800940929     C     *IN31         ANDEQ     '0'
066900940929     C                   MOVEL     ' '           V2CTRA
067000940929     C                   END
067100120925     c* Verifico se già presente particolarità restituizione doc per non
067200120925     c*  richiedere la CLI
067300120925     c                   z-add     1             y
067400120925     c                   do        30            y
067500120925     c                   if        vcp(y)<>*blanks
067600120925     C     VCP(Y)        lookup    C7Rbam                                 32
067700120925     c                   if        *in32
067800120925     c                   eval      Giadoc='S'
067900120925     c                   leave
068000120925     c                   endif
068100120925     c                   endif
068200120925     c                   enddo
068300940929     C*
068400940929     C                   ENDSR
068500940929     C*----------------------------------------------------*
068600940929     C*  CTR01  Controlli prima videata                    *
068700940929     C*----------------------------------------------------*
068800940929     C     CTR01         BEGSR
068900940929     C*
069000940929     C                   SETOFF                                       90
069100150812     C                   MOVEA     VFLP          SELPAR
069200940929     C*   Se richiesta ricerca richiamo pgm.
069300940929     C     '?'           SCAN      V1CCLI                                 32
069400940929     C     *IN32         IFEQ      '1'
069500150812     C     v1ccli        orne      *blank
069600150812     C     *in32         andne     '1'
069700150812     C     selpar        andeq     *blank
069800150812     c                   if        not *in32
069900150812     c                   movel     v1ccli        pcli
070000150812     c                   endif
070100940929     C                   MOVEA     *BLANKS       VFLP
070200940929     C                   SETON                                        90
070300050112     C                   CALL      'TRTB71R1'
070400050112     C                   PARM                    KNSIF
070500050112     C                   PARM                    PCLI              8
070600050112     C                   PARM                    PRAG             25
070700170126     C                   PARM                    KNMUS            10
070800170126     C                   PARM                    V1CRSU           20
070900050112     C                   MOVEL     PCLI          V1CCLI
071000050112     C     PCLI          IFNE      *BLANKS
071100050112     C                   MOVE      PCLI          WTPPAR            1
071200940929     C                   Z-ADD     1             X
071300940929     C     WTPPAR        LOOKUP    KPAR(X)                                32
071400940929     C   32              MOVEL     '1'           VFLP(X)
071500940929     C                   END
071600170804     ***C                   MOVEL     RAGUT         V1CRSU
071700170804     C                   MOVEL     RsUt          V1CRSU
071800940929     C                   END
071900940929     C*  Controllo attendibilità codice cliente
072000940929     C     V1CCLI        IFEQ      *ZEROS
072100940929     C     V1CCLI        OREQ      *BLANKS
072200940929     C                   SETON                                        4028
072300940929     C                   MOVEL     ERR(1)        $MSG
072400940929     C                   GOTO      FINCT1
072500940929     C                   ELSE
072600940929     C                   MOVE      V1CCLI        KKSC
072700980317     C                   CLEAR                   TIBS69
072800980317     C                   CLEAR                   CNACO
072900980317     C                   CLEAR                   CNIND
073000980317     C                   CLEAR                   CNCLP
073100980317     C                   CLEAR                   FNCLS
073200980317     C                   MOVEL     ' '           I69TLA
073300980317     C                   MOVEL     KNSIF         I69SIF
073400980317     C                   MOVE      V1CCLI        I69KAC
073500980317     C                   CALL      'TIBS69R'
073600980317     C                   PARM                    TIBS69
073700980317     C                   PARM                    CNACO
073800980317     C                   PARM                    CNIND
073900980317     C                   PARM                    CNCLP
074000980317     C                   PARM                    FNCLS
074100980317     C     O69ERR        IFNE      *BLANK
074200940929     C                   SETON                                        4028
074300980317     C                   MOVEL     O69MSG        $MSG
074400940929     C                   GOTO      FINCT1
074500940929     C                   ELSE
074600940929     C                   MOVEL     ACORAG        V1DCLI
074700940929     C                   END
074800940929     C                   END
074900940929     C*  Controllo sel.particolarità
075000940929     C                   MOVEA     VFLP          SELPAR            4
075100940929     C     SELPAR        IFEQ      *BLANKS
075200940929     C                   SETON                                        414243
075300060307     C                   SETON                                        4428
075400940929     C                   MOVEL     ERR(3)        $MSG
075500940929     C                   END
075600940929     C*
075700940929     C     FINCT1        ENDSR
075800940929     C*----------------------------------------------------*
075900940929     C*  CTR02 : Controlli seconda videata                 *
076000940929     C*----------------------------------------------------*
076100940929     C     CTR02         BEGSR
076200120925     c                   clear                   chiamacli         1
076300940929     C*
076400940929     C*  Controllo codici particolarità
076500940929     C                   MOVEA     VCP           COMO60           60
076600090224     C*
076700940929     C     COMO60        IFEQ      *BLANKS
076800090224     C*
076900050506     C     V2CSRS        ifne      'S'
077000060307     C     V2Cldcr       andle     '00'
077100940929     C                   SETON                                        28
077200940929     C                   DO        30            Y                 2 0
077300940929     C                   MOVEL     IN1(Y)        C                 2 0
077400940929     C                   MOVEL     '1'           *IN(C)
077500940929     C                   MOVEL     ERR(5)        $MSG
077600940929     C                   END
077700050506     C                   END
077800940929     C*
077900940929     C                   ELSE
078000090224     C*
078100940929     C                   DO        30            Y
078200090224     C*
078300940929     C     VCP(Y)        IFNE      *BLANKS
078400940929     C     '?'           SCAN      VCP(Y)                                 33
078500940930     C     *IN33         IFEQ      '1'
078600940930     C                   MOVEL     DSTB71        SAVDS           100
078700940930     C                   MOVEL     *BLANKS       KPJBU
078800940930     C                   MOVEL     DSTB71        KPJBU
078900940930     C                   CALL      PGMPAR
079000940930     C                   PARM                    KPJBA
079100940930     C                   MOVEL     KPJBU         DSTB71
079200940930     C                   MOVEL     DS7COD        VCP(Y)
079300940930     C                   MOVEL     SAVDS         DSTB71
079400940930     C                   END
079500090224     C*
079600090224     C     VCP(Y)        LOOKUP    CPA                                    32
079700940929     C     *IN32         IFEQ      '0'
079800940929     C                   SETON                                        28
079900940929     C                   MOVEL     IN1(Y)        C                 2 0
080000940929     C                   MOVEL     '1'           *IN(C)
080100940929     C                   MOVEL     ERR(4)        $MSG
080200090224     C                   Else
080300090224      *
080400090224      * Controlla se invierà il VAG
080500090224     c                   z-add     1             Xy                3 0
080600090224     C     VCP(Y)        lookup    C7R(Xy)                                32
080700090224      **
080800090224      * se abilitato invio del VAG
080900090224     C   32              if        c7rVAG(Xy) = 'S'
081000090224      **
081100090224      **  Controlla x il cliente sulla 3K se abilitato invio VAG
081200090224     c                   z-add     1             Xx
081300090224     C     v1cCLI        lookup    C3K(Xx)                                32
081400090224     C     *in32         ifeq      '1'
081500090224     c                   movel(p)  D3K(Xx)       ds3K
081600090224      *
081700090224      *  se manca il flag x invio VAG sulla 3K segnala l'errore
081800090224     c                   if        §3KNMC <> 'S'
081900090224     C                   SETON                                        28
082000090224     C                   MOVEL     IN1(Y)        C                 2 0
082100090224     C                   MOVEL     '1'           *IN(C)
082200090224     C                   MOVEL     ERR(7)        $MSG
082300090224     C                   End
082400090224     C                   Else
082500090224      **  Manca il cliente sulla 3K
082600090224     C                   SETON                                        28
082700090224     C                   MOVEL     IN1(Y)        C                 2 0
082800090224     C                   MOVEL     '1'           *IN(C)
082900090224     C                   MOVEL     ERR(8)        $MSG
083000090224     C                   End
083100090224      **
083200090224     C                   End
083300120925     c
083400120925     C     VCP(Y)        lookup    C7Rbam                                 32
083500120925     c                   if        *in32
083600120925     c                   eval      chiamacli='S'
083700120925     c                   endif
083800120302
083900120302       //?Controllo se cliente abilitato a tab OSR
084000120302     c                   IF        vcp(y) = 'RC'
084100120302     c                   exsr      ctrOSR
084200120302     c                   IF        not wokOSR
084300120302     c                   eval      *in28 = *on
084400120302     c                   eval      c = in1(y)
084500120302     c                   eval      *in(c) = '1'
084600120302     c                   eval      $msg = err(9)
084700120302     c                   ENDIF
084800120302     c                   ENDIF
084900090224      *
085000940929     C                   END
085100940929     C                   END
085200090224     C*
085300940929     C                   END
085400940929     C*
085500940929     C                   END
085600940929     C*
085700040806     C* CONTROLLO CHE SOLO PER LE PARTICOLARITÀ GIACENZA SI INDICHI
085800040806     C* SE IL CLIENTE VUOLE IL RESO ANCHE CON LE TRAZIONI RIDOTTE
085900040806     C     V2CSRS        IFEQ      'S'
086000040806     C     KPAR(X)       ANDNE     'Q'
086100040806     C                   SETON                                        28
086200040806     C                   MOVEL     ERR(6)        $MSG
086300040806     C                   ENDIF
086400060307      * giorni personalizzati per limite aperture con data consegna
086500060307      * richiesta validi solo per particolarità giacenze
086600060307     c                   if        kpar(x) <> 'Q' and
086700060615     c                              v2cldcr <> *blank and v2cldcr <> *zero
086800060307     C                   SETON                                        28
086900060307     C                   MOVEL     ERR(6)        $MSG
087000060307     C                   ENDIF
087100940929     C                   ENDSR
087200940929     C*----------------------------------------------------*
087300940929     C*  AGGIOR: Aggiornamento paramteri                   *
087400940929     C*----------------------------------------------------*
087500940929     C     AGGIOR        BEGSR
087600940929     C*------------------------------------------------------*
087700940929     C*  Procedo all'aggiornamento della tabella
087800940929     C     KTAB2         CHAIN     TABEL00F                           31
087900940929     C                   MOVEA     VCP           §7UCPA
088000960228     C                   MOVEL     V2CSTB        §7USTB
088100040806     C                   MOVEL     V2CSRS        §7USRE
088200060307     C                   MOVEL     V2Cldcr       §7Uldcr
088300960228     C                   MOVEL     *BLANKS       TBLUNI
088400960228     C                   MOVEL     DS7U          TBLUNI
088500940929     C     V2CTRA        IFEQ      ' '
088600940929     C                   MOVEL     '1'           TBLFTT
088700940929     C                   ELSE
088800940929     C                   MOVEL     ' '           TBLFTT
088900940929     C                   END
089000960307     C                   CLEAR                   TBLFTR
089100960307     C                   CLEAR                   TBLDTR
089200170804      /free
089300170804         if  %parms() > 1  and  TBLflg <> *blank;
089400170804           clear  TBLflg;
089500170804         endif;
089600940929     C*
089700940929     C     *IN31         IFEQ      '1'
089800940929     C                   Z-ADD     1             TBLKUT
089900940929     C                   MOVEL     '7U'          TBLCOD
090000940929     C                   MOVEL     *BLANKS       TBLKEY
090100960212     C                   MOVEL     KKEY          TBLKEY
090200940929     C                   WRITE     TABEL
090300940929     C                   ELSE
090400940929     C                   UPDATE    TABEL
090500940929     C                   END
090600170120     C*----------------------------------------------------*
090700170120     C                   ENDSR
090800170120     C*--------------------------------------------------------------*
090900170120     C*  aggiorX
091000170120     C*--------------------------------------------------------------*
091100170120     C     AGGIORx       BEGSR
091200170120     C*--
091300170120     c     Ktab2         chain     tabelx0f
091400170120      *
091500170120     c                   movel     Tabelds       xTabelds
091600170120      *
091700170120     c                   if        %found(tabelx0f)
091800170120     c                   update    tabelx
091900170120     c                   else
092000170120     c                   write     tabelx
092100170120     c                   endif
092200170120     C*--
092300940929     C                   ENDSR
092400940929     C*--------------------------------------------------------------*
092500940929     C*  ANNUL: Annullamento                                         *
092600940929     C*--------------------------------------------------------------*
092700940929     C     ANNUL         BEGSR
092800940929     C*----------------------------------------------------*
092900940929     C     KTAB2         CHAIN     TABEL00F                           80
093000940929     C     *IN80         IFEQ      '0'
093100940929     C                   MOVEL     '*'           TBLFLG
093200970204     C     V2CTRA        IFEQ      ' '
093300970204     C                   MOVEL     '1'           TBLFTT
093400970204     C                   ELSE
093500970204     C                   MOVEL     ' '           TBLFTT
093600970204     C                   END
093700970204     C                   CLEAR                   TBLFTR
093800970204     C                   CLEAR                   TBLDTR
093900940929     C                   EXCEPT    ANNTBL
094000940929     C                   END
094100940929     C*----------------------------------------------------*
094200940929     C                   ENDSR
094300940929     C*--------------------------------------------------------------*
094400940929     C*  RIPRI: Ripristino                                           *
094500940929     C*--------------------------------------------------------------*
094600940929     C     RIPRIS        BEGSR
094700940929     C*----------------------------------------------------*
094800940929     C     KTAB2         CHAIN     TABEL00F                           80
094900940929     C     *IN80         IFEQ      '0'
095000940929     C                   MOVEL     ' '           TBLFLG
095100970204     C     V2CTRA        IFEQ      ' '
095200970204     C                   MOVEL     '1'           TBLFTT
095300970204     C                   ELSE
095400970204     C                   MOVEL     ' '           TBLFTT
095500970204     C                   END
095600970204     C                   CLEAR                   TBLFTR
095700970204     C                   CLEAR                   TBLDTR
095800940929     C                   EXCEPT    ANNTBL
095900940929     C                   END
096000940929     C*----------------------------------------------------*
096100940929     C                   ENDSR
096200940929     C*----------------------------------------------------*
096300940929     C*  *INZSR: Operazioni iniziali                       *
096400940929     C*----------------------------------------------------*
096500940929     C     *INZSR        BEGSR
096600940929     C*
096700940929     C     *ENTRY        PLIST
096800940929     C                   PARM                    KPJBA
096900170804     c                   parm                    TNTB30ds
097000170804      *
097100170804      * -?Reperimento dati job?
097200170804     c                   exsr      sr_DatiJob
097300170804     c                   movel     RsUt          V1Crsu
097400170804      *
097500170804     ***C*  Richiamo XPARUT
097600170804     ***C                   Z-ADD     1             CODUT
097700170804     ***C                   CALL      'XPARUT'
097800170804     ***C                   PARM                    UTEDSE
097900170804     ***C     CODUT         CABEQ     -1            FINE
098000170804     ***C                   MOVEL     RAGUT         V1CRSU
098100170804     ***C*--- RICERCA CAPOCONTI------------------------------------------*
098200170804     ***C                   DO        50            X
098300170804     ***C                   MOVE      TCU(X)        TCUDS
098400170804     ***C     F56           IFEQ      'CG'
098500170804     ***C     F4            ANDEQ     '1'
098600170804     ***C     F3            ANDEQ     '0'
098700170804     ***C                   Z-ADD     KCU(X)        CLIITA            4 0
098800170804     ***C                   END
098900170804     ***C                   END
099000931209     C*----------------------------------------------------*
099100931209     C* Definizione chiavi                                 *
099200940929     C     KACO          KLIST
099300940929     C                   KFLD                    KKUT
099400940929     C                   KFLD                    KKCC
099500940929     C                   KFLD                    KKSC
099600170804     ***C                   Z-ADD     CLIITA        KKCC
099700170804     c                   Z-ADD     DUTkci        KKCC
099800940929     C*                                                    *
099900940927     C     KTAB1         KLIST
100000940927     C                   KFLD                    KKUT
100100940927     C                   KFLD                    KCOD
100200940929     C*                                                    *
100300940927     C     KTAB2         KLIST
100400940927     C                   KFLD                    KKUT
100500940927     C                   KFLD                    KCOD
100600940927     C                   KFLD                    KKEY
100700931209     C*----------------------------------------------------*
100800931209     C* Definizione variabili                              *
100900940929     C     *LIKE         DEFINE    ACOKCC        KKCC
101000940929     C     *LIKE         DEFINE    ACOKSC        KKSC
101100940927     C     *LIKE         DEFINE    TBLKUT        KKUT
101200940927     C     *LIKE         DEFINE    TBLCOD        KCOD
101300940927     C     *LIKE         DEFINE    TBLKEY        KKEY
101400971210     C******************************************************
101500971210     C* Richiamo pgm per controllo modificabilità tabella
101600120302      * non faccio controllo per s.i. di prova
101700120302     c                   IF        %subst(knsif:7:1) <> 'P'
101800971210     C                   CLEAR                   TRUL12
101900971210     C                   MOVEL     'L'           I12TLA
102000971210     C                   MOVEL     KNSIF         I12NSI
102100971210     C                   MOVEL     '7U'          I12COD
102200971210     C                   CALL      'TRUL12R'
102300971210     C                   PARM                    TRUL12
102400971210     C     O12ERR        IFEQ      '1'
102500040806     C*****                GOTO FINE
102600971210     C                   END
102700120302     c                   ENDIF
102800931209     C*----------------------------------------------------*
102900940929     C*  Caricamento tabella 7P
103000940927     C                   Z-ADD     1             KKUT
103100940929     C                   MOVE      '7P'          KCOD
103200120925     C                   Z-ADD     0             X
103300160726     C     KTAB1         CHAIN(n)  TABEL00F                           31
103400940927     C     *IN31         DOWEQ     '0'
103500110210     C     X             ANDLT     9999
103600940929     C     TBLFLG        IFEQ      ' '
103700940725     C                   ADD       1             X
103800940929     C                   MOVE      TBLKEY        C7P(X)
103900940929     C                   END
104000160726     C     KTAB1         READE(n)  TABEL00F                               31
104100940725     C                   END
104200940927     C*----------------------------------------------------*
104300940929     C*  Caricamento tabella 7Q
104400120925     C                   Z-ADD     0             X
104500940929     C                   MOVEL     '7Q'          KCOD
104600160726     C     KTAB1         CHAIN(n)  TABEL                              31
104700940927     C     *IN31         DOWEQ     '0'
104800110210     C     X             ANDLT     9999
104900940929     C     TBLFLG        IFEQ      ' '
105000940927     C                   ADD       1             X
105100940929     C                   MOVEL     TBLKEY        C7Q(X)
105200940927     C                   END
105300160726     C     KTAB1         READE(n)  TABEL                                  31
105400940927     C                   END
105500940929     C*----------------------------------------------------*
105600940929     C*  Caricamento tabella 7R
105700120925     C                   Z-ADD     0             X                 5 0
105800120925     C                   Z-ADD     0             xx                5 0
105900940929     C                   MOVEL     '7R'          KCOD
106000160726     C     KTAB1         CHAIN(n)  TABEL                              31
106100940929     C     *IN31         DOWEQ     '0'
106200110210     C     X             ANDLT     9999
106300940929     C     TBLFLG        IFEQ      ' '
106400940929     C                   ADD       1             X
106500940929     C                   MOVEL     TBLKEY        C7R(X)
106600090224     c                   movel(p)  tblUNI        ds7R
106700090224     c                   if        §7R7T1 <> *blank or
106800090224     c                             §7R7T2 <> *blank or
106900090224     c                             §7R7T3 <> *blank or
107000090224     c                             §7R7T4 <> *blank or
107100090224     c                             §7R7T5 <> *blank
107200090224      * abilitato invio del VAG
107300090224     c                   move      'S'           C7Rvag(X)
107400940929     C                   END
107500120925     c
107600120925     c* controllo se ha la restituzione dei documenti
107700120925     c                   if        §7r1bf<>' '
107800120925     c                   add       1             xx
107900120925     c                   movel     tblkey        C7Rbam(xx)
108000120925     c                   endif
108100120925     c
108200090224     C                   END
108300160726     C     KTAB1         READE(n)  TABEL                                  31
108400940929     C                   END
108500940929     C*----------------------------------------------------*
108600940929     C*  Caricamento tabella 7S
108700120925     C                   Z-ADD     0             X
108800940929     C                   MOVEL     '7S'          KCOD
108900160726     C     KTAB1         CHAIN(n)  TABEL                              31
109000940929     C     *IN31         DOWEQ     '0'
109100110210     C     X             ANDLT     9999
109200940929     C     TBLFLG        IFEQ      ' '
109300940929     C                   ADD       1             X
109400940929     C                   MOVEL     TBLKEY        C7S(X)
109500940929     C                   END
109600160726     C     KTAB1         READE(n)  TABEL                                  31
109700940929     C                   END
109800940929     C*----------------------------------------------------*
109900090224     C*  Caricamento tabella 3K
110000090225     C                   Z-ADD     0             Xx                5 0
110100090224     C                   MOVEL     '3K'          KCOD
110200160726     C     KTAB1         CHAIN(n)  TABEL                              31
110300090224     C     *IN31         DOWEQ     '0'
110400090224     C     Xx            ANDLT     9999
110500090224     C     TBLFLG        IFEQ      ' '
110600090225     C                   ADD       1             Xx
110700090224     C                   MOVEL     TBLKEY        C3K(Xx)
110800090224     C                   MOVEL     TBLuni        D3K(Xx)
110900090224     C                   END
111000160726     C     KTAB1         READE(n)  TABEL                                  31
111100090224     C                   END
111200090224     C*----------------------------------------------------*
111300940929     C                   MOVEA     C7P           COMO(1)
111400940929     C                   MOVEA     C7Q           COMO(2)
111500940929     C                   MOVEA     C7R           COMO(3)
111600940929     C                   MOVEA     C7S           COMO(4)
111700940929     C*  Inizializzo DS per call pgm. particolarità
111800940930     C                   CLEAR                   DSTB71
111900940930     C                   MOVEL     'S'           DS7RIC
112000940930     C                   MOVEL     'V'           DS7GES
112100940928     C*----------------------------------------------------*
112200940929     C                   ENDSR
112300120302
112400120302      /free
112500170804
112600170804       //--------------------------------------------------------------
112700170804       //?Reperimento Dati del job (Utente/Operativi).                 ?
112800170804       //--------------------------------------------------------------
112900170804       BEGSR  sr_DatiJob;
113000170804
113100170804         in(e) §AzUte;
113200170804         if NOT %error;
113300170804           in(e) §DatiUte;
113400170804         endif;
113500170804         if %error or RSut = *blank;
113600170804           tibs34r ( tibs34ds );
113700170804           in §AzUte;
113800170804           in §DatiUte;
113900170804         endif;
114000170804
114100170804       ENDSR;
114200170804
114300120302       //--------------------------------------------------------------
114400120302       //?Controllo se cliente abilitato alla OSR
114500120302       //--------------------------------------------------------------
114600120302       BEGSR CtrOSR;
114700120302
114800120302       //?Controllo se cliente abilitato a tab OSR
114900120302         wokOSR = *off;
115000120302         setll kTBEcod TNTBE01L;
115100120302         reade kTBEcod TNTBE01L;
115200120302         DOW not %eof(TNTBE01L);
115300120302           dOSR = TBEuni;
115400120302           IF  d§OSRcli  = V2Ccli or
115500120302               d§OSRcl2  = V2Ccli or
115600120302               d§OSRcl3  = V2Ccli or
115700120302               d§OSRcl4  = V2Ccli or
115800120302               d§OSRcl5  = V2Ccli or
115900120302               d§OSRcl6  = V2Ccli or
116000120302               d§OSRcl7  = V2Ccli or
116100120302               d§OSRcl8  = V2Ccli or
116200120302               d§OSRcl9  = V2Ccli or
116300120302               d§OSRcl10 = V2Ccli or
116400120302               d§OSRcl11 = V2Ccli or
116500120302               d§OSRcl12 = V2Ccli or
116600120302               d§OSRcl13 = V2Ccli or
116700120302               d§OSRcl14 = V2Ccli or
116800120302               d§OSRcl15 = V2Ccli or
116900120302               d§OSRcl16 = V2Ccli;
117000120302             wokOSR = *on;
117100120302           ELSE;
117200120302             wokOSR = *off;
117300120302           ENDIF;
117400120302
117500120302           IF  wokOSR;
117600120302             leave;
117700120302           ENDIF;
117800120302           reade kTBEcod TNTBE01L;
117900120302         ENDDO;
118000120302
118100120302       ENDSR;
118200120302      /end-free
118300120302
118400931209     OTABEL     E            ANNTBL
118500931209     O                       TBLFLG
118600970204     O                       TBLFTT
118700970204     O                       TBLFTR
118800970204     O                       TBLDTR
118900160901     OTABEL     E            VUOTO
119000090224**  MESSAGGI DI ERRORE   (ERR)
119100940929Immettere codice cliente                                              01
119200940929Codice cliente inesistente                                            02
119300940929Effettuare almeno una scelta                                          03
119400940929Codice particolarità inesistente                                      04
119500940929Immettere almeno un codice particolarità                              05
119600040806Immettere SOLO PER PARTICOLARITA GIACENZE !                           06
119700090224Su tab.3K non è selezionato l'invio con VAG delle Mancate Consegne !  07
119800090224Manca la tab.3K codificata per il cliente x l'invio del VAG !         08
119900120302Cliente non abilitato in tabella OSR                                  09
120000940929**  MODALITA' PGM.
120100940419IMMISSIONE
120200940419VARIAZIONE
120300940929ANNULLATO
120400940929**  Descrizioni Particolarità gestite
120500940929 PARTICOLARITA' CONTRASSEGNI
120600940929   PARTICOLARITA' GIACENZE
120700940929   PARTICOLARITA' CONSEGNE
120800940929PARTICOLARITA' VARIE ALL'ARRIVO
120900940929**  Codice particolarità
121000940929PQRS
121100940929**  PGM. gestione particolarità
121200940929TRTB72R
121300940929TRTB69R
121400940929TRTB70R
121500940929TRTB73R
121600940929** Indicatori Particolarità
121700940929404142434445464748495051525354555657585960616263646566676869
