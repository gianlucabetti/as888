000100970526     H*------------------------------------------------------------------------*
000200161129     H DECEDIT('0,') DATEDIT(*DMY.) dftactgrp(*NO) bnddir('UBBNDDIR')
000300970526     F*------------------------------------------------------------------------*
000400970526     F* DATA BASE
000500970526     F*------------------------------------------------------------------------*
000600161124     FTIS7VCP1D CF   E             WORKSTN INFDS(DEVDS1)
000700161124     F                                     SFILE(S7P1D05:SFLULT)
000800161124     F                                     SFILE(S7P1DMS:MSGULT)
000900970526     F*------------
001000970526     F* VERSIONI
001100970526     F*------------
001200161128     FTIVCP01I  IF   E           K DISK    rename(TIVCP000:TIVCP01)
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D*------------
001700970526     D* DECODIFICHE
001800970526     D*------------
001900970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* MESSAGGI
002200970526     D*------------
002300970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* COMANDI
002600970526     D*------------
002700970526     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI OPZIONI
003000970526     D*------------
003100970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* DESCRIZIONI TASTI FUNZIONALI
003400970526     D*------------
003500970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* OPZIONI DA CARICARE A VIDEO
003800970526     D*------------
003900970526     D OPV             S             19    DIM(24)
004000970526     D*------------
004100970526     D* FUNZIONI DA CARICARE A VIDEO
004200970526     D*------------
004300970526     D FUV             S             19    DIM(24)
004400970526     D*------------------------------------------------------------------------*
004500970526     D* INPUT
004600970526     D*------------------------------------------------------------------------*
004700970526     D*------------
004800970526     D* RIDEFINIZIONE OPZIONI VIDEO
004900970526     D*------------
005000970526     D OPDDS           DS
005100970526     D  OPDOPZ                 1      2
005200970526     D  OPDDE1                 3     19
005300970526     D  OPDOP1                21     22
005400970526     D  OPDFI0                23     34
005500970526     D  OPDATT                35     35
005600970526     D  OPDFI1                36     80
005700970526     D  OPDDES                 1     19
005800970526     D  OPDRIG                 1     80
005900970526     D*------------
006000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006100970526     D*------------
006200970526     D FUDDS           DS
006300970526     D  FUDDE1                 1      1
006400970526     D  FUDFUN                 2      3
006500970526     D  FUDDE2                 4     19
006600970526     D  FUDFI0                20     34
006700970526     D  FUDATT                35     35
006800970526     D  FUDFI1                36     80
006900970526     D  FUDDES                 1     19
007000970526     D  FUDRIG                 1     80
007100970526     D*------------
007200970526     D* DS GESTIONE TASTI FUNZIONALI
007300970526     D*------------
007400970526     D DSKEY         E DS
007500970526     D DEVDS1          DS
007600970526     D  KEY                  369    369
007700970526     D*------------
007800970526     D* DS "XMSG" - GESTIONE MESSAGGI
007900970526     D*------------
008000970526     D DSMSG         E DS
008100970526     D*------------
008200020919     D* DS REPERIMENTO DATI UTENTE
008300970526     D*------------
008400020919     D TIBS34DS      E DS                                                       *Profili utente
008500020919     D DDATIUTE      E DS                                                       *Dati utente
008600020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008700970526     D*------------------
008800970526     D* DS "XSRDA8" - CONTROLLA DATA (8)
008900970526     D*------------------
009000970526     D WLBDA8          DS                  INZ
009100970526     D  G08DAT                 1      8  0
009200970526     D  G08INV                 9     16  0
009300970526     D  G08ERR                17     17
009400970526     D  G08TGI                18     22  0
009500970526     D*------------
009600161123     D* DS DI PROCEDURA
009700970526     D*------------
009800161128     D TIS7VCP1DS    E DS
009900161128     D TIS7VCP2DS    E DS
010000970526     D*------------
010100970526     D* ARCHITETTURA
010200970526     D*------------
010300970526     D KPJBA         E DS
010400970715     D  UTENTE                 1      3
010500160713     D*------------
010600160713     D* campi di appoggio
010700160713     D*------------
010800161128     D DAnn            S                   like(VCPAnn   )
010900170413     D DTPID           S                   like(VCPTPID  )
011000170413     D DEPTPID         S                   like(D03TPID  )
011100170413     D DID             S                   like(VCPID    )
011200170413     D DEPID           S                   like(D03ID    )
011300160831     D*------------
011400160831     D* campi tipo data per le date a video
011500160831     D ValidISO        s               d
011600160913     D DDEISO          s               d
011700160831       // - Stringa SQL da eseguire
011800160831     d wSQl            s           1024    inz  varying
011900161129     d wPrg            s             15s 0
012000170413     d wPrgDal         s             15s 0
012100170413     d wIncr           s              3s 0
012200170413     d esito           s              2s 0
012300161129
012400161129       //--------------------------------------------------------------
012500161129       // Definizione prototipi procedure.
012600161129       //--------------------------------------------------------------
012700161129
012800170426       /copy gaitrasrc/srcProtoPr,UBVCP0R_01
012900170426
013000170426       /copy gaitrasrc/srcProtoPr,UBVCP0R_04
013100161129
013200970526     C*------------------------------------------------------------------------*
013300970526     C* MAIN LINES
013400970526     C*------------------------------------------------------------------------*
013500970526     C*
013600970526     C* SE NON CI SONO ERRORI, PROSEGUE
013700970526IF  1C     ERRL00        IFNE      '1'
013800970526     C*
013900970526     C* CARICA IL SFILE
014000970526     C                   EXSR      POSSFL
014100970526     C*
014200970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014300970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014600970526     C*
014700970526     C* REIMPOSTA ERRORE DI PROCERURA
014800970526     C                   MOVEL     '0'           ERRL00
014900970526     C*
015000970526     C* EMETTE IL SFILE
015100970526     C                   EXSR      OUTSFL
015200970526     C*
015300970526     C* GESTIONE TASTI FUNZIONALI
015400970526     C                   EXSR      FUNCON
015500970526     C*
015600970526E   2C                   ENDDO
015700970526E   1C                   ENDIF
015800970526     C* FINE PGM
015900970526     C                   SETON                                        LR
016000970526     C*------------------------------------------------------------------------*
016100970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
016200970526     C*------------------------------------------------------------------------*
016300970526     C     F03GES        BEGSR
016400970526     C*
016500970526     C* IMPOSTA "PREMUTO F03"
016600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016700161128     C                   MOVEL     '1'           VCP1F03                         *DI PROCEDURA
016800160916     C*
016900970526     C                   ENDSR
017000970526     C*------------------------------------------------------------------------*
017100970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
017200970526     C*------------------------------------------------------------------------*
017300970526     C     F05GES        BEGSR
017400160923     C*
017500160923     c                   eval      FUNGru = 0
017600160923     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
017700970526     C*
017800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017900970526     C*
018000970526     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
018300970526     C*------------------------------------------------------------------------*
018400970526     C     F06GES        BEGSR
018500970526     C*
018600970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
018700020919IF  1C     DUTLPO        IFNE      'S'
018800970526     C                   MOVEL     '1'           ERRL00
018900970526     C                   MOVEL     MSG(3)        DSMSMS
019000970526E   1C                   ENDIF
019100970526     C*
019200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019300161128     C                   CLEAR                   DAnn
019400170413     C                   CLEAR                   DTPID
019500170413     C                   CLEAR                   DID
019600970529     C                   MOVEL     DEPOP0        WOP0
019700970529     C                   MOVEL     'F06'         WOP1
019800161124     C                   MOVEL     'TIS7VCP2R'   WPGM             10
019900970526     C                   EXSR      LANPGM                                       *LANCIA PGM
020000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
020100970526E   1C                   ENDIF
020200970526     C*
020300970526     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500970526     C* F12GES - GESTIONE F12 -> RITORNO
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F12GES        BEGSR
020800970526     C*
020900970526     C* IMPOSTA "PREMUTO F12"
021000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021100161128     C                   MOVEL     '1'           VCP1F12                         *DI PROCEDURA
021200970526     C*
021300970526     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500161121     C* F21GES - GESTIONE F21 -> GESTIONE STAMPE
021600970526     C*------------------------------------------------------------------------*
021700161121     C     F21GES        BEGSR
021800970526     C*
021900161123     C*
022000970526     C                   ENDSR
022100970526     C*------------------------------------------------------------------------*
022200970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
022300970526     C*------------------------------------------------------------------------*
022400970526     C     F23GES        BEGSR
022500970526     C*
022600970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022700970526     C*
022800970526     C                   ENDSR
022900970526     C*------------------------------------------------------------------------*
023000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023100970526     C*------------------------------------------------------------------------*
023200970526     C     F24GES        BEGSR
023300970526     C*
023400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023500970526     C*
023600970526     C                   ENDSR
023700970526     C*------------------------------------------------------------------------*
023800970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023900970526     C*------------------------------------------------------------------------*
024000970526     C     RUPGES        BEGSR
024100970526     C*
024200970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
024300970526     C*
024400970526     C                   ENDSR
024500970526     C*------------------------------------------------------------------------*
024600970526     C* ENTGES - GESTIONE ENTER -> INVIO
024700970526     C*------------------------------------------------------------------------*
024800970526     C     ENTGES        BEGSR
024900970526     C*
025000970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025100970526     C                   EXSR      NEWPOS
025200970526     C*
025300970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025500970526IF  2C     SWPOSI        IFNE      0
025600970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
025700970526X   2C                   ELSE
025800970526IF  3C     SFLULT        IFNE      0
025900970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026000970526     C*
026100970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026200970526IF  4C     SWMODI        IFNE      0
026300970526     C                   EXSR      POSSFL
026400970526E   4C                   ENDIF
026500970526E   3C                   ENDIF
026600970526E   2C                   ENDIF
026700970526E   1C                   ENDIF
026800970526     C*
026900970526     C                   ENDSR
027000970526     C*------------------------------------------------------------------------*
027100970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
027200970526     C*------------------------------------------------------------------------*
027300970526     C     O01GES        BEGSR
027400970526     C*
027500160923     C*
027600970526     C                   ENDSR
027700970526     C*------------------------------------------------------------------------*
027800970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
027900970526     C*------------------------------------------------------------------------*
028000970526     C     O02GES        BEGSR
028100970526     C*
028200970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
028300161128IF  1C     D05Ann        IFEQ      'A'
028400970526     C                   MOVEL     '1'           ERRL00
028500970526     C                   MOVEL     MSG(4)        DSMSMS
028600970526E   1C                   ENDIF
028700970526     C* MODIFICA
028800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028900161128     C                   EVAL      DAnn        = D05Ann
029000170413     C                   EVAL      DTPID       = D05TPID
029100170413     C                   EVAL      DID         = D05ID
029200970529     C                   MOVEL     DEPOP0        WOP0
029300970526     C                   MOVEL     'O02'         WOP1
029400161124     C                   MOVEL     'TIS7VCP2R'   WPGM
029500970526     C                   EXSR      LANPGM
029600171020     C                   Z-ADD     1             SWMODI                        *FLAG RICARICA SFL
029700970526E   1C                   ENDIF
029800970526     C*
029900160927     C                   ENDSR
030000160927     C*
030100160927     C*------------------------------------------------------------------------*
030200160927     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
030300160927     C*------------------------------------------------------------------------*
030400160927     C     O03GES        BEGSR
030500160927     C*
030600160914     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
030700161128IF  1C     D05Ann        IFNE      *blank
030800160914     C                   MOVEL     '1'           ERRL00
030900160914     C                   MOVEL     MSG(4)        DSMSMS
031000160914E   1C                   ENDIF
031100160914     C* COPIA
031200160914IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031300161128     C                   EVAL      DAnn        = D05Ann
031400170413     C                   EVAL      DTPID       = D05TPID
031500170413     C                   EVAL      DID         = D05ID
031600160914     C                   MOVEL     DEPOP0        WOP0
031700160914     C                   MOVEL     'O03'         WOP1
031800161124     C                   MOVEL     'TIS7VCP2R'   WPGM
031900160914     C                   EXSR      LANPGM
032000171020     C                   Z-ADD     1             SWMODI                        *FLAG RICARICA SFL
032100160914E   1C                   ENDIF
032200160914     C*
032300160914     C                   ENDSR
032400970526     C*------------------------------------------------------------------------*
032500970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
032600970526     C*------------------------------------------------------------------------*
032700970526     C     O04GES        BEGSR
032800160902     C*
032900160902     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
033000161128IF  1C     D05Ann        IFNE      *blank
033100160902     C                   MOVEL     '1'           ERRL00
033200160902     C                   MOVEL     MSG(4)        DSMSMS
033300160902E   1C                   ENDIF
033400970526     C*
033500970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
033600160902IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033700161128     C                   EVAL      DAnn        = D05Ann
033800170413     C                   EVAL      DTPID       = D05TPID
033900170413     C                   EVAL      DID         = D05ID
034000970529     C                   MOVEL     DEPOP0        WOP0
034100970526     C                   MOVEL     'O04'         WOP1
034200161124     C                   MOVEL     'TIS7VCP2R'   WPGM
034300970526     C                   EXSR      LANPGM
034400171020     C                   Z-ADD     1             SWMODI                        *FLAG RICARICA SFL
034500970526E   1C                   ENDIF
034600970526     C*
034700970526     C                   ENDSR
034800970526     C*------------------------------------------------------------------------*
034900970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
035000970526     C*------------------------------------------------------------------------*
035100970526     C     O05GES        BEGSR
035200970526     C*
035300161128     C                   EVAL      DAnn        = D05Ann
035400170413     C                   EVAL      DTPID       = D05TPID
035500170413     C                   EVAL      DID         = D05ID
035600970529     C                   MOVEL     DEPOP0        WOP0
035700970526     C                   MOVEL     'O05'         WOP1
035800161124     C                   MOVEL     'TIS7VCP2R'   WPGM
035900970526     C                   EXSR      LANPGM
036000970526     C*
036100970526     C                   ENDSR
036200970526     C*------------------------------------------------------------------------*
036300970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
036400970526     C*------------------------------------------------------------------------*
036500970526     C     O07GES        BEGSR
036600970526     C*
036700170428     C                   EVAL      DAnn        = D05Ann
036800170428     C                   EVAL      DTPID       = D05TPID
036900170428     C                   EVAL      DID         = D05ID
037000170428     C                   MOVEL     DEPOP0        WOP0
037100170428     C                   MOVEL     'O07'         WOP1
037200170428     C                   MOVEL     'TIS7VCP2R'   WPGM
037300170428     C                   EXSR      LANPGM
037400171020     C                   Z-ADD     1             SWMODI                        *FLAG RICARICA SFL
037500171020     C*
037600970526     C                   ENDSR
037700161129     C*------------------------------------------------------------------------*
037800161129     C* O13GES - Usa Contatore
037900161129     C*------------------------------------------------------------------------*
038000161129     C     O13GES        BEGSR
038100161129     C*
038200170307          if d03NrPrg = 0;
038300170413            ubvcp0r_prg(d05TpID:d05ID:esito:wPrg);
038400170307          else;
038500170413            ubvcp0r_prg(d05TpID:d05ID:esito:wPrg:d03NrPrg:wPrgDal:wIncr);
038600170307          endif;
038700170413
038800170426     C* EMISSIONE RISULTATO
038900170426          if esito = 0;
039000170426     C                   MOVEL     MSG(14)       DSMSMS
039100171020     C                   Z-ADD     1             SWMODI                        *FLAG RICARICA SFL
039200170426          else;
039300170426     C                   MOVEL     MSG(15)       DSMSMS
039400170426          endif;
039500161129     C*
039600161129     C                   ENDSR
039700170426     C*------------------------------------------------------------------------*
039800170426     C* O14GES - Controlla Contatore
039900170426     C*------------------------------------------------------------------------*
040000170426     C     O14GES        BEGSR
040100170426     C*
040200170524          if d03PrgCtl = 0;
040300170524            ubvcp0r_ctl(d05TpID:d05ID:d05Anno:esito);
040400170524          else;
040500170524            ubvcp0r_ctl(d05TpID:d05ID:d05Anno:esito:d03PrgCtl);
040600170524          endif;
040700170426
040800170426     C* EMISSIONE RISULTATO
040900170426          if esito = 0;
041000170426     C                   MOVEL     MSG(16)       DSMSMS
041100170426          else;
041200170426            DSMSMS = %trim(MSG(17)) + ' ' + %char(esito);
041300170426          endif;
041400170426     C*
041500170426     C                   ENDSR
041600970526     C*------------------------------------------------------------------------*
041700970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
041800970526     C*------------------------------------------------------------------------*
041900970526     C     NEWPOS        BEGSR
042000970526     C*
042100970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
042200970526     C*         1= RICHIESTO POSIZIONAMENTO
042300970526     C                   Z-ADD     0             SWPOSI            7 0
042400970526     C*
042500970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
042600970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
042700970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
042800970526     C                   Z-ADD     0             SWMODI            1 0
042900970526     C*
043000970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
043100170413IF  1C     D03TPID       IFNE      DEPTPID
043200170413IF  1C     D03ID         ORNE      DEPID
043300970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
043400970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
043500970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
043600170413     C                   EVAL      DEPTPID = D03TPID
043700170413     C                   EVAL      DEPID   = D03ID
043800970526E   2C                   ENDIF
043900970526E   1C                   ENDIF
044000970526     C*
044100970526     C                   ENDSR
044200970526     C*------------------------------------------------------------------------*
044300970526     C* SETFIL - POSIZIONAMENTO RECORD
044400970526     C*------------------------------------------------------------------------*
044500970526     C     SETFIL        BEGSR
044600970526     C*
044700970526     C* POSIZIONAMENTO DEL FILE E PRIMA LETTURA IN BASE AL TIPO DI RICERCA
044800160830     C                   SELECT
044900160831IF  1C     WTIPRI        WHENEQ    'ALL'
045000161128     C     *loval        SETLL     TIVCP01I                           99
045100160830     C*
045200161128IF  1C     WTIPRI        WHENEQ    'ID '
045300170413     C***  D03ID         SETLL     TIVCP01I                           99
045400170413IF  2      setll  (D03TpID:D03ID)  TIVCP01;
045500170413           if %eof;
045600170413             *in99=*on;
045700170413           else;
045800170413             *in99=*off;
045900170413           endif;
046000161128     C*
046100160830E   1C                   ENDSL
046200970526     C* LETTURA RECORD
046300970526IF  1C     *IN99         IFEQ      *OFF
046400970526     C                   EXSR      LETFIL
046500970526E   1C                   ENDIF
046600970526     C*
046700970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
046800970526IF  1C     *IN99         IFEQ      *ON
046900970526     C                   Z-ADD     1             SFLEOF
047000970526E   1C                   ENDIF
047100970526     C*
047200970526     C                   ENDSR
047300970526     C*------------------------------------------------------------------------*
047400970526     C* LETFIL - LETTURA RECORD
047500970526     C*------------------------------------------------------------------------*
047600970526     C     LETFIL        BEGSR
047700970526     C*
047800970526     C* LEGGE FINO A:
047900970526     C*     - FINE FILE
048000970526     C*     - TROVATO RECORD VALIDO
048100970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
048200970526DO  1C     *IN99         DOWEQ     *OFF
048300970526     C     WRECOK        ANDEQ     'N'
048400970526     C*
048500970526     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
048600160830     C                   SELECT
048700160831IF  1C     WTIPRI        WHENEQ    'ALL'
048800161128     C                   READ      TIVCP01I                               99
048900160830     C*
049000161128IF  1C     WTIPRI        WHENEQ    'ID '
049100170413     C                   READ      TIVCP01I                               99
049200161128     C*
049300160830E   2C                   ENDSL
049400970526     C*
049500970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
049600970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
049700970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
049800970530X   2C                   ELSE
049900970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
050000970530E   2C                   ENDIF
050100970526E   1C                   ENDDO
050200970526     C*
050300970526     C                   ENDSR
050400970526     C*------------------------------------------------------------------------*
050500970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
050600970526     C*------------------------------------------------------------------------*
050700970526     C     CHKREC        BEGSR
050800970526     C*
050900970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
051000160713
051100970526     C                   ENDSR
051200970526     C*------------------------------------------------------------------------*
051300160830     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE            §
051400970526     C*------------------------------------------------------------------------*
051500970526     C     IRESFL        BEGSR
051600970526     C*
051700970526     C                   MOVEL     *BLANKS       OPZKEY
051800161129     C                   EVAL      D05Ann    = VCPAnn
051900170413     C                   EVAL      D05TpID   = VCPTpID
052000170413     C                   EVAL      D05ID     = VCPID
052100161128     C                   EVAL      D05Anno   = VCPAnno
052200161128     C                   EVAL      D05Prg    = VCPPrg
052300170413     C                   EVAL      D05From   = VCPFrom
052400170413     C                   EVAL      D05To     = VCPTo
052500170413     C                   EVAL      D05MaxPrg = VCPMaxPrg
052600161128     C                   EVAL      D05Incr   = VCPIncr
052700160901     C*
052800970526     C                   ENDSR
052900970526     C*------------------------------------------------------------------------*
053000970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
053100970526     C*------------------------------------------------------------------------*
053200970526     C     CHKPAR        BEGSR
053300970526     C*
053400970526     C* REIMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
053500970526     C                   SETOFF                                       808182
053600160913     C                   SETOFF                                       838485
053700970526     C*--------------------
053800970526     C* ORDINAMENTO
053900970526     C*--------------------
054000970526     C*--------------------
054100970526     C* SELEZIONI
054200970526     C*--------------------
054300970526     C*
054400160831     C* SE NESSUN ORDINAMENTO RICHIESTO, utilizzo il caricamento completo per Procedura
054500160831     C                   MOVEL     'ALL'         WTIPRI            3
054600970526     C*
054700161128IF  1C     ERRL00        IFEQ      '0'
054800161128     C                   SELECT
054900170413IF  2C     D03TPID       WHENNE    *blank
055000170413IF  2C     D03ID         ORNE      *blank
055100161128     C                   MOVEL     'ID '         WTIPRI
055200161128     C                   OTHER
055300161128     C                   MOVEL     'ALL'         WTIPRI
055400161128E   2C                   ENDSL
055500161128E   1C                   ENDIF
055600970526     C*
055700970526     C                   ENDSR
055800970526     C*------------------------------------------------------------------------*
055900970526     C* LANPGM - LANCIA UN PROGRAMMA
056000970526     C*------------------------------------------------------------------------*
056100970526     C     LANPGM        BEGSR
056200970526     C*
056300970526     C* IMPOSTA LA DS DI PROCEDURA
056400161128     C                   MOVEL     WOP0          VCP2OP0                         *LIVELLO PROCEDURA
056500161128     C                   MOVEL     WOP1          VCP2OP1                         *LIVELLO PGM
056600161128     C                   MOVEL     '0'           VCP2F03                         *PREMUTO F03=FINE
056700161128     C                   MOVEL     '0'           VCP2F12                         *PREMUTO F12=RIT
056800161128     C                   MOVEL     '0'           VCP2ERR                         *'1'=ERRORE
056900161128     C                   MOVEL     *BLANKS       VCP2MSG                         *MESSAGGIO DI RIT
057000161128     C                   EVAL      VCP2ANN    = DAnn
057100170413     C                   EVAL      VCP2TPID   = DTPID
057200170413     C                   EVAL      VCP2ID     = DID
057300970526     C*
057400970526     C                   CALL      WPGM
057500970612     C                   PARM                    KPJBA
057600161128     C                   PARM                    TIS7VCP2DS
057700970526     C*
057800970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
057900161128IF  1C     VCP1F03       IFEQ      '1'                                          *PREMUTO F03
058000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
058100970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
058200970526X   1C                   ELSE                                                   *NON PREMUTO F03
058300161128     C                   MOVEL     VCP2MSG       DSMSMS                         *MESSAGGIO DI RIT
058400970526E   1C                   ENDIF
058500970526     C*
058600970526     C                   ENDSR
058700970527     C*------------------------------------------------------------------------*
058800970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
058900970527     C*------------------------------------------------------------------------*
059000970527     C     CARTAB        BEGSR
059100970527     C*
059200970527     C                   ENDSR
059300970526     C*------------------------------------------------------------------------*
059400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
059500970526     C*------------------------------------------------------------------------*
059600970526     C     CHKFOP        BEGSR
059700970626     C*
059800970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
059900970626IF  1C     DEPOP0        IFEQ      'R01'
060000970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
060100970626     C                   MOVEL     'S'           OPDATT
060200970626     C                   MOVEL     OPDRIG        OPD(1)
060300970626X   1C                   ELSE
060400970626     C* OPZIONI
060500970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
060600970626     C                   MOVEL     'S'           OPDATT
060700970626     C                   MOVEL     OPDRIG        OPD(5)
060800970626     C* FUNZIONI
060900970626E   1C                   ENDIF
061000970626     C*
061100000602     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
061200970626IF  1C     DEPOP0        IFEQ      'R02'
061300161128     C*                  MOVEL     OPD(1)        OPDRIG                         *MODIFICA
061400161128     C*                  MOVEL     'S'           OPDATT
061500161128     C*                  MOVEL     OPDRIG        OPD(1)
061600160923     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
061700160923     C                   MOVEL     'S'           OPDATT
061800160923     C                   MOVEL     OPDRIG        OPD(2)
061900970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
062000970626     C                   MOVEL     'S'           OPDATT
062100970626     C                   MOVEL     OPDRIG        OPD(4)
062200010702     C*                    MOVELOPD,6     OPDRIG           *ALLINEAMENTO
062300010702     C*                    MOVEL'S'       OPDATT
062400010702     C*                    MOVELOPDRIG    OPD,6
062500970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
062600970626     C                   MOVEL     'S'           OPDATT
062700970626     C                   MOVEL     OPDRIG        OPD(7)
062800970715     C*
062900161129     C* SOLO SE EDP ABILITA COPIA E INSERIMENTO
063000970715IF  2C     UTENTE        IFEQ      'EDP'
063100970715     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
063200970715     C                   MOVEL     'S'           OPDATT
063300970715     C                   MOVEL     OPDRIG        OPD(3)
063400970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
063500970626     C                   MOVEL     'S'           FUDATT
063600970626     C                   MOVEL     FUDRIG        FUD(6)
063700970715E   2C                   ENDIF
063800170426       // solo per gli utenti VAS abilito:
063900170426       // - l'opzione di uso Contatore
064000170426       // - il tasto di funzione di inserimento automatico
064100161129       if knmus = 'EDPLC' or knmus = 'EDPFG';
064200170426     C                   MOVEL     OPD(13)       OPDRIG
064300161129     C                   MOVEL     'S'           OPDATT
064400161129     C                   MOVEL     OPDRIG        OPD(13)
064500170426     C                   MOVEL     FUD(16)       FUDRIG
064600170426     C                   MOVEL     'S'           FUDATT
064700170426     C                   MOVEL     FUDRIG        FUD(16)
064800161129       endif;
064900161129
065000970715E   1C                   ENDIF
065100970626     C*
065200000602     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO, ALLINEAMENTO
065300020919IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
065400970626     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
065500970626     C                   MOVEL     'N'           OPDATT
065600970626     C                   MOVEL     OPDRIG        OPD(4)
065700010702     C*                    MOVELOPD,6     OPDRIG           *NO ALLINEAMENTO
065800010702     C*                    MOVEL'N'       OPDATT
065900010702     C*                    MOVELOPDRIG    OPD,6
066000970626     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
066100970626     C                   MOVEL     'N'           OPDATT
066200970626     C                   MOVEL     OPDRIG        OPD(7)
066300970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
066400970626     C                   MOVEL     'N'           FUDATT
066500970626     C                   MOVEL     FUDRIG        FUD(6)
066600970626E   1C                   ENDIF
066700970526     C*
066800970526     C* GESTIONE OPZIONI A VIDEO
066900970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
067000970526     C                   EXSR      OPZGES
067100970526     C*
067200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
067300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
067400970526     C                   EXSR      FUNGES
067500970526     C*
067600970526     C                   ENDSR
067700020919     C*--------------------------------------------------------------------------------------------*
067800020919     C* REPERISCE I DATI UTENTE
067900020919     C*--------------------------------------------------------------------------------------------*
068000020919     C     REPDATIUTE    BEGSR
068100020919     C*
068200020919     C* INIZIALIZZA VARIABILI DI WRK
068300020919     C                   CLEAR                   TIBS34DS
068400020919     C                   CLEAR                   AZUTEDS
068500020919     C                   CLEAR                   DDATIUTE
068600020919     C*
068700020919     C     *DTAARA       DEFINE    §azute        azuteds
068800020919     C     *DTAARA       DEFINE    §datiute      ddatiute
068900020919     C                   IN(E)     *DTAARA
069000020919if  1C                   IF        %Error
069100020919     c                   EVAL      I34Tla = 'L'
069200020919     C                   CALL      'TIBS34R'
069300020919     C                   PARM                    Tibs34Ds
069400020919     C                   IN        *DTAARA
069500020919e   1C                   ENDIF
069600020919     C*
069700020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
069800020919     C                   MOVEL(P)  RSUT          DSFIRS           20
069900020919     C*
070000020919     C                   ENDSR
070100970526     C*------------------------------------------------------------------------*
070200970526     C* *INZSR - OPERAZIONI INIZIALI
070300970526     C*------------------------------------------------------------------------*
070400970526     C     *INZSR        BEGSR
070500970526     C*---
070600970526     C* RICEVIMENTO PARAMETRI
070700970526     C*---
070800970526     C     *ENTRY        PLIST
070900970612     C                   PARM                    KPJBA
071000160714     C*
071100161128     C                   EVAL      TIS7VCP1DS = kpjbu
071200160913     c                   if        kpjbu = *blank
071300161128     c                   clear                   TIS7VCP1DS
071400160913     c                   endif
071500970526     C*
071600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
071700970526     C     XMSGPA        PLIST
071800970526     C                   PARM                    DSMSMS
071900970526     C                   PARM                    DSMSPG
072000970526     C                   PARM                    DSMSKE
072100970526     C                   PARM                    DSMSER
072200970526     C*
072300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
072400970526     C                   CALL      'XKEY'
072500970526     C                   PARM                    DSKEY
072600970526     C*---
072700970526     C* VARIABILI NON RIFERITE AL DATA BASE
072800970526     C*---
072900970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
073000970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
073100970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
073200970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
073300970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
073400970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
073500970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
073600970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
073700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
073800161128     C     *LIKE         DEFINE    VCP1OP0       DEPOP0                         *DI DEPOSITO
073900161128     C     *LIKE         DEFINE    VCP1OP1       DEPOP1
074000161128     C     *LIKE         DEFINE    VCP1OP0       WOP0                           *DI LAVORO
074100161128     C     *LIKE         DEFINE    VCP1OP1       WOP1
074200970526     C*---
074300970526     C* VARIABILI DI CONTROLLO
074400970526     C*---
074500970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
074600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
074700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
074800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
074900970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
075000170413     C                   Z-ADD     13            SFLPAG                         *N°RIGHE X PAG
075100970526     C*
075200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
075300161124     C                   MOVEL     'TIS7VCP1R'   DSMSPG
075400161124     C                   MOVEL     'TIS7VCP1R'   MSGQUE
075500970526     C                   MOVEL     *BLANKS       MSGKEY
075600970526     C                   MOVEL     *BLANKS       DSMSMS
075700970526     C                   MOVEL     *BLANKS       DSMSKE
075800970526     C                   Z-ADD     *ZEROS        DSMSER
075900970526     C*---
076000970526     C* VARIABILI RIFERITE AL DATA BASE
076100970526     C*---
076200970526     C*---
076300970526     C* CHIAVI DI LETTURA
076400970526     C*---
076500970527     C*
076600970527     C* REPERIMENTO DATI UTENTE
076700020919     C                   EXSR      REPDATIUTE
076800970527     C*
076900970527     C* CARICA LE TABELLE OCCORRENTI
077000970527     C                   EXSR      CARTAB
077100970526     C*
077200970526     C* DEPOSITA LE OPZIONI DI ENTRATA
077300161128     C                   MOVEL     VCP1OP0       DEPOP0                         *LIVELLO PROCEDURA
077400161128     C                   MOVEL     VCP1OP1       DEPOP1                         *LIVELLO PROGRAMMA
077500970526     C*
077600970526     C* CALCOLA LA DATA CORRENTE
077700970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
077800970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
077900970526     C                   Z-ADD     WN8           G08DAT
078000970526     C                   Z-ADD     *ZEROS        G08INV
078100970526     C                   MOVEL     '0'           G08ERR
078200970526     C                   CALL      'XSRDA8'
078300970526     C                   PARM                    WLBDA8
078400970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
078500970526     C*
078600970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
078700970526     C                   EXSR      CHKFOP
078800970526     C*
078900970526     C* IMPOSTA I PARAMETRI DI INGRESSO
079000160912     C*  intanto imposto le selezioni con i dati passati
079100170413     C                   EVAL      D03TPID = VCP1TPID
079200170413     C                   EVAL      D03ID   = VCP1ID
079300160714     C                   EXSR      CHKPAR
079400160714IF  1C     ERRL00        IFEQ      '1'
079500161128     C                   MOVEL     '1'           VCP1ERR
079600161128     C                   MOVEL     DSMSMS        VCP1MSG
079700160714X   1C                   ELSE
079800160714E   1C                   ENDIF
079900980225     C*
080000970526     C*
080100970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
080200970530IF  1C     DEPOP0        IFEQ      'R01'
080300970530     C                   MOVEL     DEC(3)        VIDING
080400970530E   1C                   ENDIF
080500970530IF  1C     DEPOP0        IFEQ      'R02'
080600970530     C                   MOVEL     DEC(1)        VIDING
080700970530E   1C                   ENDIF
080800970530IF  1C     DEPOP0        IFEQ      'R05'
080900970530     C                   MOVEL     DEC(2)        VIDING
081000970530E   1C                   ENDIF
081100970526     C*
081200970526     C                   ENDSR
081300970526     C*------------------------------------------------------------------------*
081400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
081500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
081600970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
081700970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
081800970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
081900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
082000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
082100970526     C*------------------------------------------------------------------------*
082200970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
082300970526     C*------------------------------------------------------------------------*
082400970526     C     POSSFL        BEGSR
082500970526     C*
082600970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
082700970526     C                   EXSR      CLRSFL
082800970526     C*
082900970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
083000161129     C***                EXSR      SETFIL
083100970526     C*
083200970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
083300970526     C     SFLEOF        IFEQ      0
083400970526     C                   EXSR      CARSFL
083500970526     C                   END
083600970526     C*
083700970526     C                   ENDSR
083800970526     C*------------------------------------------------------------------------*
083900970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
084000970526     C*------------------------------------------------------------------------*
084100970526     C     ENTATT        BEGSR
084200970526     C*
084300970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
084400970526     C                   Z-ADD     0             OPZOK             1 0
084500970526     C*
084600970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
084700970526     C                   Z-ADD     0             SFLVAR            5 0
084800970526     C*
084900970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
085000970526     C                   Z-ADD     0             SFLERR
085100970526     C*
085200970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
085300970526     C                   Z-ADD     SFLULT        WFLULT
085400970526     C*
085500970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
085600161124     C                   READC     S7P1D05                                98
085700970526     C     *IN98         DOWEQ     '0'
085800970526     C*
085900970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
086000970526     C                   ADD       1             SFLVAR
086100970526     C*
086200970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
086300970526     C     OPZOK         IFEQ      0
086400970526     C*
086500970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
086600970526     C     OPZKEY        IFNE      *BLANKS
086700970526     C                   EXSR      OPZCON
086800970526     C                   END
086900970526     C*
087000970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
087100970526     C*            2. PULIZIA SCELTA IN SFL
087200970526     C     OPZOK         IFEQ      0
087300970526     C                   MOVEL     '0'           *IN41
087400970526     C                   MOVEL     *BLANKS       OPZKEY
087500970526     C                   ELSE
087600970526     C*
087700970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
087800970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
087900970526     C                   MOVEL     '1'           *IN41
088000970526     C                   Z-ADD     SFLULT        SFLERR
088100970526     C                   END
088200970526     C                   ELSE
088300970526     C*
088400970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
088500970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
088600970526     C                   MOVEL     '1'           *IN41
088700970526     C                   END                                                    OPZOK=0
088800970526     C*
088900970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
089000970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
089100970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
089200970526     C     OPZOK         IFNE      0
089300970526     C                   Z-ADD     SFLERR        SFLNBR
089400970526     C                   ELSE
089500970526     C                   Z-ADD     SFLULT        SFLNBR
089600970526     C                   END
089700970526     C*
089800970526     C* AGGIORNO RECORD SFL
089900160902     C*  imposto "effetti speciali"
090000160902     C                   EXSR      SFXD05
090100161124     C                   UPDATE    S7P1D05
090200970526     C*
090300161124     C                   READC     S7P1D05                                98
090400970526     C                   END                                                    DO WHILE
090500970526     C*
090600970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
090700970526     C                   Z-ADD     WFLULT        SFLULT
090800970526     C*
090900970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
091000970526     C     SFLVAR        IFEQ      0
091100970526     C*---                 MOVEL'1'       FINL00
091200970526     C                   END
091300970526     C*
091400970526     C                   ENDSR
091500970526     C*------------------------------------------------------------------------*
091600970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
091700970526     C*------------------------------------------------------------------------*
091800970526     C     CLRSFL        BEGSR
091900970526     C*
092000970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
092100970526     C* (0=NON FINE FILE  1=FINE FILE)
092200970526     C                   Z-ADD     0             SFLEOF
092300970526     C*
092400970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
092500970526     C                   Z-ADD     0             SFLULT
092600970526     C*
092700970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
092800970526     C                   MOVEL     '1'           *IN40
092900161124     C                   WRITE     S7P1D06
093000970526     C                   MOVEL     '0'           *IN40
093100970526     C*
093200970526     C                   ENDSR
093300970526     C*------------------------------------------------------------------------*
093400970526     C* CARSFL - CARICAMENTO SFL
093500970526     C*------------------------------------------------------------------------*
093600970526     C     CARSFL        BEGSR
093700970526     C*
093800970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
093900970526     C     SFLULT        IFEQ      0
094000970526     C                   EXSR      SETFIL
094100970526     C                   END
094200970526     C*
094300970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
094400970526     C                   Z-ADD     SFLULT        WFLULT
094500970526     C*
094600970526     C* CICLO:
094700970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
094800970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
094900970526     C                   Z-ADD     1             SFLRIG
095000970526     C     SFLRIG        DOWLE     SFLPAG
095100970526     C     SFLEOF        ANDEQ     0
095200970526     C*
095300970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
095400970526     C                   MOVEL     *BLANKS       OPZKEY
095500970526     C*
095600970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
095700970526     C                   EXSR      IRESFL
095800970526     C*
095900970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
096000970526     C                   ADD       1             SFLULT
096100970526     C*
096200970526     C* SCRITTURA RECORD SFL
096300160902     C*  imposto "effetti speciali"
096400160902     C                   EXSR      SFXD05
096500161124     C                   WRITE     S7P1D05
096600970526     C                   ADD       1             SFLRIG
096700970526     C*
096800970526     C* LETTURA FILE DA CARICARE
096900970526     C                   EXSR      LETFIL
097000970526     C                   END                                                    DO WHILE
097100970526     C*
097200970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
097300970526     C     WFLULT        IFNE      SFLULT
097400970526     C                   Z-ADD     WFLULT        SFLNBR
097500970526     C                   ADD       1             SFLNBR
097600970526     C                   END
097700970526     C*
097800970526     C                   ENDSR
097900970526     C*------------------------------------------------------------------------*
098000160902     C* SFXD05 - effetti speciali sfl D05 (RI, ND, BL, ...)
098100160902     C*          a seconda dei dati presenti in riga
098200970526     C*------------------------------------------------------------------------*
098300160902     C     SFXD05        BEGSR
098400970526     C*
098500160902     C* RI campi chiave per rcd annullato
098600160902     C                   SETOFF                                       01
098700161128     C                   IF        D05Ann <> *blank
098800160902     C                   SETON                                        01
098900160902     C                   ENDIF
099000160902     C*
099100160902     C                   ENDSR
099200160902     C*------------------------------------------------------------------------*
099300160902     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
099400160902     C*------------------------------------------------------------------------*
099500160902     C     OUTSFL        BEGSR
099600160902     C*
099700970526     C* WRITE RECORD TESTATA VIDEO
099800161124     C                   WRITE     S7P1D01
099900970526     C*
100000970526     C* VISUALIZZAZIONE MESSAGGIO
100100970526     C                   EXSR      SNDMSG
100200970526     C*
100300970526     C* WRITE RECORD OPZIONI VIDEO
100400161124     C                   WRITE     S7P1D02
100500970526     C*
100600970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
100700161124     C                   WRITE     S7P1D03
100800970526     C*
100900970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
101000161124     C                   WRITE     S7P1D04
101100970526     C*
101200970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
101300970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
101400970526     C     SFLEOF        IFEQ      0
101500970526     C                   MOVEL     '0'           *IN42
101600970526     C                   ELSE
101700970526     C*
101800970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
101900970526     C                   MOVEL     '1'           *IN42
102000970526     C                   END
102100970526     C*
102200970526     C* WRITE RECORD TASTI FUNZIONALI
102300161124     C                   WRITE     S7P1D09
102400970526     C*
102500970526     C* CASO DI SFL VUOTO
102600970526     C     SFLULT        IFLE      0
102700970526     C*
102800970526     C*    - WRITE RECORD VUOTO
102900161124     C                   WRITE     S7P1D0A
103000161124     C                   READ      S7P1D0A                                99
103100970526     C*
103200970526     C                   ELSE
103300970526     C*
103400970526     C* CASO DI SFL CON RECORDS
103500970526     C*
103600970526     C*    - WRITE RECORD CONTROLLO SFL
103700161124     C                   WRITE     S7P1D06
103800161124     C                   READ      S7P1D06                                99
103900970526     C                   END
104000970526     C*
104100970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
104200161124     C                   READ      S7P1D03                                99
104300970526     C*
104400970526     C                   ENDSR
104500970526     C*------------------------------------------------------------------------*
104600970526     C* SNDMSG - MANDA MESSAGGIO
104700970526     C*------------------------------------------------------------------------*
104800970526     C     SNDMSG        BEGSR
104900970526     C*
105000970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
105100970526     C                   MOVEL     '1'           *IN40
105200161124     C                   WRITE     S7P1DMC
105300970526     C                   MOVEL     '0'           *IN40
105400970526     C*
105500970526     C* VISUALIZZAZIONE MESSAGGIO
105600970526     C                   Z-ADD     0             MSGULT
105700970526     C                   CALL      'XMSG'        XMSGPA
105800970526     C     DSMSER        IFEQ      0
105900970526     C                   MOVEL     DSMSKE        MSGKEY
106000970526     C                   END
106100970526     C                   Z-ADD     1             MSGULT
106200161124     C                   WRITE     S7P1DMS                                      SFL MESS
106300161124     C                   WRITE     S7P1DMC                                      CONTR MESS
106400970526     C*
106500970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
106600970526     C                   MOVEL     *BLANKS       DSMSMS
106700970526     C*
106800970526     C                   ENDSR
106900970526     C*------------------------------------------------------------------------*
107000970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
107100970526     C*------------------------------------------------------------------------*
107200970526     C     FUNCON        BEGSR
107300970526     C*
107400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
107500970526     C                   EXSR      FUNVAR
107600970526     C*
107700970526     C* PULISCO CAMPI DI LAVORO
107800970526     C                   Z-ADD     0             I                 5 0
107900970526     C                   Z-ADD     0             W                 5 0
108000970526     C*
108100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
108200970526     C     FUNKEY        IFNE      *BLANKS
108300970526     C     1             DO        24            I
108400970526     C                   MOVEL     FUD(I)        FUDRIG
108500970526     C*
108600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
108700970526     C     FUNKEY        IFEQ      FUDFUN
108800970526     C     FUDATT        ANDEQ     'S'
108900970526     C                   Z-ADD     I             W
109000970526     C                   END
109100970526     C                   END                                                    DO WHILE
109200970526     C*
109300970526     C* FUNZIONE NON ATTIVA (ERRORE)
109400970526     C     W             IFEQ      0
109500970526     C*
109600970526     C* EMISSIONE MESSAGGIO DI ERRORE
109700970526     C                   MOVEL     MSG(2)        DSMSMS
109800970526     C                   END                                                    W=0
109900970526     C                   END                                                    FUNKEY NE *BLANKS
110000970526     C*
110100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
110200970526     C     W             IFNE      0
110300970526     C     FUNKEY        OREQ      *BLANKS
110400970526     C*
110500970526     C     FUNKEY        IFEQ      *BLANKS
110600970526     C                   EXSR      ENTGES
110700970526     C                   END
110800970526     C     FUNKEY        IFEQ      '03'
110900970526     C                   EXSR      F03GES
111000970526     C                   END
111100970526     C     FUNKEY        IFEQ      '05'
111200970526     C                   EXSR      F05GES
111300970526     C                   END
111400970526     C     FUNKEY        IFEQ      '06'
111500970526     C                   EXSR      F06GES
111600970526     C                   END
111700160913     C     FUNKEY        IFEQ      '12'
111800160913     C                   EXSR      F12GES
111900160913     C                   END
112000161128     C     FUNKEY        IFEQ      '23'
112100161128     C                   EXSR      F23GES
112200970526     C                   END
112300970526     C     FUNKEY        IFEQ      '24'
112400970526     C                   EXSR      F24GES
112500970526     C                   END
112600970526     C     FUNKEY        IFEQ      'RU'
112700970526     C                   EXSR      RUPGES
112800970526     C                   END
112900970526     C*
113000970526     C                   END
113100970526     C*
113200970526     C                   ENDSR
113300970526     C*------------------------------------------------------------------------*
113400970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
113500970526     C*------------------------------------------------------------------------*
113600970526     C     FUNVAR        BEGSR
113700970526     C*
113800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
113900970526     C     KEY           IFEQ      ENTER
114000970526     C                   MOVEL     *BLANKS       FUNKEY
114100970526     C                   END
114200970526     C     KEY           IFEQ      F1
114300970526     C                   MOVEL     '01'          FUNKEY
114400970526     C                   END
114500970526     C     KEY           IFEQ      F2
114600970526     C                   MOVEL     '02'          FUNKEY
114700970526     C                   END
114800970526     C     KEY           IFEQ      F3
114900970526     C                   MOVEL     '03'          FUNKEY
115000970526     C                   END
115100970526     C     KEY           IFEQ      F4
115200970526     C                   MOVEL     '04'          FUNKEY
115300970526     C                   END
115400970526     C     KEY           IFEQ      F5
115500970526     C                   MOVEL     '05'          FUNKEY
115600970526     C                   END
115700970526     C     KEY           IFEQ      F6
115800970526     C                   MOVEL     '06'          FUNKEY
115900970526     C                   END
116000970526     C     KEY           IFEQ      F7
116100970526     C                   MOVEL     '07'          FUNKEY
116200970526     C                   END
116300970526     C     KEY           IFEQ      F8
116400970526     C                   MOVEL     '08'          FUNKEY
116500970526     C                   END
116600970526     C     KEY           IFEQ      F9
116700970526     C                   MOVEL     '09'          FUNKEY
116800970526     C                   END
116900970526     C     KEY           IFEQ      F10
117000970526     C                   MOVEL     '10'          FUNKEY
117100970526     C                   END
117200970526     C     KEY           IFEQ      F11
117300970526     C                   MOVEL     '11'          FUNKEY
117400970526     C                   END
117500970526     C     KEY           IFEQ      F12
117600970526     C                   MOVEL     '12'          FUNKEY
117700970526     C                   END
117800970526     C     KEY           IFEQ      F13
117900970526     C                   MOVEL     '13'          FUNKEY
118000970526     C                   END
118100970526     C     KEY           IFEQ      F14
118200970526     C                   MOVEL     '14'          FUNKEY
118300970526     C                   END
118400970526     C     KEY           IFEQ      F15
118500970526     C                   MOVEL     '15'          FUNKEY
118600970526     C                   END
118700970526     C     KEY           IFEQ      F16
118800970526     C                   MOVEL     '16'          FUNKEY
118900970526     C                   END
119000970526     C     KEY           IFEQ      F17
119100970526     C                   MOVEL     '17'          FUNKEY
119200970526     C                   END
119300970526     C     KEY           IFEQ      F18
119400970526     C                   MOVEL     '18'          FUNKEY
119500970526     C                   END
119600970526     C     KEY           IFEQ      F19
119700970526     C                   MOVEL     '19'          FUNKEY
119800970526     C                   END
119900970526     C     KEY           IFEQ      F20
120000970526     C                   MOVEL     '20'          FUNKEY
120100970526     C                   END
120200970526     C     KEY           IFEQ      F21
120300970526     C                   MOVEL     '21'          FUNKEY
120400970526     C                   END
120500970526     C     KEY           IFEQ      F22
120600970526     C                   MOVEL     '22'          FUNKEY
120700970526     C                   END
120800970526     C     KEY           IFEQ      F23
120900970526     C                   MOVEL     '23'          FUNKEY
121000970526     C                   END
121100970526     C     KEY           IFEQ      F24
121200970526     C                   MOVEL     '24'          FUNKEY
121300970526     C                   END
121400970526     C     KEY           IFEQ      ROLLUP
121500970526     C                   MOVEL     'RU'          FUNKEY
121600970526     C                   END
121700970526     C     KEY           IFEQ      HELP
121800970526     C                   MOVEL     'HP'          FUNKEY
121900970526     C                   END
122000970526     C*
122100970526     C                   ENDSR
122200970526     C*------------------------------------------------------------------------*
122300970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
122400970526     C*------------------------------------------------------------------------*
122500970526     C     FUNGES        BEGSR
122600970526     C*
122700970526     C* PULISCO CAMPI DI LAVORO
122800970526     C                   Z-ADD     0             W                 5 0
122900970526     C*
123000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
123100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
123200970526     C     1             DO        24            I
123300970526     C                   MOVEL     FUD(I)        FUDRIG
123400970526     C*
123500970526     C* PULIZIA FUNZIONI PRECEDENTI
123600970526     C                   MOVEL     *BLANKS       FUV(I)
123700970526     C*
123800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
123900970526     C* (SALTA F23 E F24)
124000970526     C     FUDATT        IFEQ      'S'
124100970526     C     FUDFUN        ANDNE     '23'
124200970526     C     FUDFUN        ANDNE     '24'
124300970526     C                   ADD       1             W
124400970526     C                   MOVEL     FUDDES        FUV(W)
124500970526     C                   END
124600970526     C                   END                                                    DO WHILE
124700970526     C*
124800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
124900970526     C* ALTRIMENTI DISATTIVO F24
125000970526     C     1             DO        24            I
125100970526     C                   MOVEL     FUD(I)        FUDRIG
125200970526     C*
125300970526     C     FUDFUN        IFEQ      '24'
125400970526     C     W             IFGT      6
125500970526     C                   MOVEL     'S'           FUDATT
125600970526     C                   ELSE
125700970526     C                   MOVEL     *BLANKS       FUDATT
125800970526     C                   END                                                    W GT 8
125900970526     C                   MOVEL     FUDRIG        FUD(I)
126000970526     C                   END
126100970526     C                   END                                                    DO WHILE
126200970526     C*
126300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
126400970526     C     W             DIV       6             I
126500970526     C                   MVR                     Z
126600970526     C*
126700970526     C     Z             IFNE      0
126800970526     C     I             ADD       1             Z
126900970526     C                   ELSE
127000970526     C                   Z-ADD     I             Z
127100970526     C                   END
127200970526     C*
127300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
127400970526     C                   ADD       1             FUNGRU
127500970526     C     FUNGRU        IFGT      Z
127600970526     C     FUNGRU        ORLT      1
127700970526     C                   Z-ADD     1             FUNGRU
127800970526     C                   END
127900970526     C*
128000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
128100970526     C     FUNGRU        IFEQ      1
128200970526     C                   MOVEL     FUV(1)        FUN001
128300970526     C                   MOVEL     FUV(2)        FUN002
128400970526     C                   MOVEL     FUV(3)        FUN003
128500970526     C                   MOVEL     FUV(4)        FUN004
128600970526     C                   MOVEL     FUV(5)        FUN005
128700970526     C                   MOVEL     FUV(6)        FUN006
128800970526     C                   END
128900970526     C*
129000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
129100970526     C     FUNGRU        IFEQ      2
129200970526     C                   MOVEL     FUV(7)        FUN001
129300970526     C                   MOVEL     FUV(8)        FUN002
129400970526     C                   MOVEL     FUV(9)        FUN003
129500970526     C                   MOVEL     FUV(10)       FUN004
129600970526     C                   MOVEL     FUV(11)       FUN005
129700970526     C                   MOVEL     FUV(12)       FUN006
129800970526     C                   END
129900970526     C*
130000970526     C* TERZO GRUPPO DI 6 FUNZIONI
130100970526     C     FUNGRU        IFEQ      3
130200970526     C                   MOVEL     FUV(13)       FUN001
130300970526     C                   MOVEL     FUV(14)       FUN002
130400970526     C                   MOVEL     FUV(15)       FUN003
130500970526     C                   MOVEL     FUV(16)       FUN004
130600970526     C                   MOVEL     FUV(17)       FUN005
130700970526     C                   MOVEL     FUV(18)       FUN006
130800970526     C                   END
130900970526     C*
131000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
131100970526     C     FUNGRU        IFEQ      4
131200970526     C                   MOVEL     FUV(19)       FUN001
131300970526     C                   MOVEL     FUV(20)       FUN002
131400970526     C                   MOVEL     FUV(21)       FUN003
131500970526     C                   MOVEL     FUV(22)       FUN004
131600970526     C                   MOVEL     *BLANKS       FUN005
131700970526     C                   MOVEL     *BLANKS       FUN006
131800970526     C                   END
131900970526     C*
132000970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
132100970526     C                   MOVEL     *BLANKS       FUN007
132200970526     C                   MOVEL     *BLANKS       FUN008
132300970526     C*
132400970526     C     1             DO        24            I
132500970526     C                   MOVEL     FUD(I)        FUDRIG
132600970526     C     FUDFUN        IFEQ      '23'
132700970526     C     FUDATT        ANDEQ     'S'
132800970526     C                   MOVEL     FUDDES        FUN007
132900970526     C                   END
133000970526     C     FUDFUN        IFEQ      '24'
133100970526     C     FUDATT        ANDEQ     'S'
133200970526     C                   MOVEL     FUDDES        FUN008
133300970526     C                   END
133400970526     C                   END                                                    DO
133500970526     C*
133600970526     C                   ENDSR
133700970526     C*------------------------------------------------------------------------*
133800970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
133900970526     C*------------------------------------------------------------------------*
134000970526     C     OPZGES        BEGSR
134100970526     C*
134200970526     C* PULISCO CAMPI DI LAVORO
134300970526     C                   Z-ADD     0             W                 5 0
134400970526     C*
134500970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
134600970526     C* E CARICO TABELLA OPZIONI ATTIVE
134700970526     C     1             DO        24            I
134800970526     C                   MOVEL     OPD(I)        OPDRIG
134900970526     C*
135000970526     C* PULIZIA OPZIONI PRECEDENTI
135100970526     C                   MOVEL     *BLANKS       OPV(I)
135200970526     C*
135300970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
135400970526     C     OPDATT        IFEQ      'S'
135500970526     C                   ADD       1             W
135600970526     C                   MOVEL     OPDDES        OPV(W)
135700970526     C                   END
135800970526     C                   END                                                    DO WHILE
135900970526     C*
136000970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
136100970526     C* ALTRIMENTI DISATTIVO F23
136200970526     C     1             DO        24            I
136300970526     C                   MOVEL     FUD(I)        FUDRIG
136400970526     C*
136500970526     C     FUDFUN        IFEQ      '23'
136600970526     C     W             IFGT      8
136700970526     C                   MOVEL     'S'           FUDATT
136800970526     C                   ELSE
136900970526     C                   MOVEL     *BLANKS       FUDATT
137000970526     C                   END                                                    W GT 8
137100970526     C                   MOVEL     FUDRIG        FUD(I)
137200970526     C                   END
137300970526     C                   END                                                    DO WHILE
137400970526     C*
137500970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
137600970526     C     W             DIV       8             I
137700970526     C                   MVR                     Z                 5 0
137800970526     C*
137900970526     C     Z             IFNE      0
138000970526     C     I             ADD       1             Z
138100970526     C                   ELSE
138200970526     C                   Z-ADD     I             Z
138300970526     C                   END
138400970526     C*
138500970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
138600970526     C                   ADD       1             OPZGRU
138700970526     C     OPZGRU        IFGT      Z
138800970526     C     OPZGRU        ORLT      1
138900970526     C                   Z-ADD     1             OPZGRU
139000970526     C                   END
139100970526     C*
139200970526     C* PRIMO GRUPPO DI 8 OPZIONI
139300970526     C     OPZGRU        IFEQ      1
139400970526     C                   MOVEL     OPV(1)        OPZ001
139500970526     C                   MOVEL     OPV(2)        OPZ002
139600970526     C                   MOVEL     OPV(3)        OPZ003
139700970526     C                   MOVEL     OPV(4)        OPZ004
139800970526     C                   MOVEL     OPV(5)        OPZ005
139900970526     C                   MOVEL     OPV(6)        OPZ006
140000970526     C                   MOVEL     OPV(7)        OPZ007
140100970526     C                   MOVEL     OPV(8)        OPZ008
140200970526     C                   END
140300970526     C*
140400970526     C* SECONDO GRUPPO DI 8 OPZIONI
140500970526     C     OPZGRU        IFEQ      2
140600970526     C                   MOVEL     OPV(9)        OPZ001
140700970526     C                   MOVEL     OPV(10)       OPZ002
140800970526     C                   MOVEL     OPV(11)       OPZ003
140900970526     C                   MOVEL     OPV(12)       OPZ004
141000970526     C                   MOVEL     OPV(13)       OPZ005
141100970526     C                   MOVEL     OPV(14)       OPZ006
141200970526     C                   MOVEL     OPV(15)       OPZ007
141300970526     C                   MOVEL     OPV(16)       OPZ008
141400970526     C                   END
141500970526     C*
141600970526     C* TERZO GRUPPO DI 8 OPZIONI
141700970526     C     OPZGRU        IFEQ      3
141800970526     C                   MOVEL     OPV(17)       OPZ001
141900970526     C                   MOVEL     OPV(18)       OPZ002
142000970526     C                   MOVEL     OPV(19)       OPZ003
142100970526     C                   MOVEL     OPV(20)       OPZ004
142200970526     C                   MOVEL     OPV(21)       OPZ005
142300970526     C                   MOVEL     OPV(22)       OPZ006
142400970526     C                   MOVEL     OPV(23)       OPZ007
142500970526     C                   MOVEL     OPV(24)       OPZ008
142600970526     C                   END
142700970526     C*
142800970526     C                   ENDSR
142900970526     C*------------------------------------------------------------------------*
143000970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
143100970526     C*------------------------------------------------------------------------*
143200970526     C     OPZCON        BEGSR
143300970526     C*
143400970526     C* PULISCO CAMPI DI LAVORO
143500970526     C                   Z-ADD     0             I                 5 0
143600970526     C                   Z-ADD     0             W                 5 0
143700970526     C*
143800970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
143900970526     C     1             DO        24            I
144000970526     C                   MOVEL     OPD(I)        OPDRIG
144100970526     C*
144200970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
144300970526     C     OPZKEY        IFEQ      OPDOPZ
144400970526     C     OPZKEY        OREQ      OPDOP1
144500970526     C*
144600970526     C     OPDATT        IFEQ      'S'
144700970526     C                   MOVEL     OPDOPZ        OPZKEY
144800970526     C                   Z-ADD     I             W
144900970526     C                   END
145000970526     C                   END
145100970526     C*
145200970526     C                   END                                                    DO WHILE
145300970526     C*
145400970526     C* OPZIONE NON ATTIVA (ERRORE)
145500970526     C     W             IFEQ      0
145600970526     C*
145700970526     C* OPZIONE NON OK
145800970526     C                   Z-ADD     1             OPZOK
145900970526     C*
146000970526     C* EMISSIONE MESSAGGIO DI ERRORE
146100970526     C                   MOVEL     MSG(1)        DSMSMS
146200970526     C                   ELSE
146300970526     C*
146400970526     C* OPZIONE ATTIVA (PROCEDI ...)
146500970526     C*
146600970526     C     OPZKEY        IFEQ      ' 1'
146700970526     C                   EXSR      O01GES
146800970526     C                   END
146900970526     C     OPZKEY        IFEQ      ' 2'
147000970526     C                   EXSR      O02GES
147100970526     C                   END
147200970603     C     OPZKEY        IFEQ      ' 3'
147300970603     C                   EXSR      O03GES
147400970603     C                   END
147500970526     C     OPZKEY        IFEQ      ' 4'
147600970526     C                   EXSR      O04GES
147700970526     C                   END
147800970526     C     OPZKEY        IFEQ      ' 5'
147900970526     C                   EXSR      O05GES
148000970526     C                   END
148100970526     C     OPZKEY        IFEQ      ' 7'
148200170428     C                   EXSR      O07GES
148300970526     C                   END
148400970528     C     OPZKEY        IFEQ      ' 8'
148500970528     C                   END
148600970528     C     OPZKEY        IFEQ      ' 9'
148700970528     C                   END
148800970528     C     OPZKEY        IFEQ      '10'
148900970528     C                   END
149000970528     C     OPZKEY        IFEQ      '11'
149100970528     C                   END
149200970603     C     OPZKEY        IFEQ      '13'
149300161129     C                   EXSR      O13GES
149400970603     C                   END
149500170426     C     OPZKEY        IFEQ      '14'
149600170426     C                   EXSR      O14GES
149700170426     C                   END
149800160916     C     OPZKEY        IFEQ      '23'
149900160916     C                   END
150000970526     C                   END                                                    OPZ.NON ATTIVA
150100970526     C*
150200970526     C                   ENDSR
150300970526     C*------------------------------------------------------------------------*
150400970526     C* NOTE COSTRUZIONE SCHIERE
150500970526     C*------------------------------------------------------------------------*
150600970526     C*
150700970526     C* -MSG-MESSAGGI
150800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
150900970526     C* (NON TOCCARE MSG,1 E MSG,2)
151000970526     C*
151100970526     C* -CMD-COMANDI
151200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
151300970526     C*
151400970526     C* -OPD-OPZIONI
151500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
151600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
151700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
151800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
151900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
152000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
152100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
152200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
152300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
152400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
152500970526     C*
152600970526     C* -FUD-FUNZIONI
152700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
152800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
152900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
153000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
153100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
153200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
153300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
153400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
153500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
153600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
153700970526     O*------------------------------------------------------------------------*
153800970526** DEC - DECODIFICHE
153900970526    GESTIONE                                                     1
154000970526INTERROGAZIONE                                                   2
154100970530     SCELTA                                                      3
1542009705264
1543009705265
154400970526** MSG - MESSAGGI
154500161124TIS7VCP1R- Opzione non gestita                                                 1
154600161124TIS7VCP1R- Tasto funzionale non gestito                                        2
154700161124TIS7VCP1R-                                                                     3
154800161124TIS7VCP1R- Record annullato.                                                   4
154900161124TIS7VCP1R- Record non annullato. Impossibile ripristinare                      5
155000161124TIS7VCP1R- Accettato solo un parametro di inizio lista                         6
155100161124TIS7VCP1R- Errori nei parametri di ingresso                                    7
155200161124TIS7VCP1R- Data errata                                                         8
155300161124TIS7VCP1R-                                                                     9
155400161124TIS7VCP1R- Errore SQL nel reperimento dati Validi al                          10
155500161124TIS7VCP1R- Copia totale (opz. 23) eseguita correttamente                      11
155600161124TIS7VCP1R- Copia totale (opz. 23) non eseguita                                12
155700161124TIS7VCP1R- Stampa effettuata                                                  13
155800171020TIS7VCP1R- Contatore aggiornato                                               14
155900170413TIS7VCP1R- Contatore non aggiornato per errore                                15
156000170426TIS7VCP1R- Contatore esistente e coerente                                     16
156100170426TIS7VCP1R- Contatore errato:                                                  17
156200161124TIS7VCP1R- 18                                                                 18
156300161124TIS7VCP1R- 19                                                                 19
156400161124TIS7VCP1R- 20                                                                 20
156500970526** CMD - COMANDI CL
156600161128
156700970526
156800970526
156900970526
157000970526
157100970526** OPD - OPZIONI
157200160923 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
157300970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
157400160914 3=Copia            3 -     (S/N)=N                        opz. 03¬
157500970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
157600970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
157700160713                   -  -     (S/N)=N                        opz. 06¬
157800170428 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
157900160713                   -  -     (S/N)=N                        opz. 08¬
158000160713                   -  -     (S/N)=N                        opz. 09¬
158100160713                   -  -     (S/N)=N                        opz. 10¬
158200160713                   -  -     (S/N)=N                        opz. 11¬
158300970526                   -  -     (S/N)=N                        opz. 12¬
15840016112913=Usa Contatore   -  -     (S/N)=N                        opz. 13¬
15850017042614=Ctrl Contatore  -  -     (S/N)=S                        opz. 14¬
158600970526                   -  -     (S/N)=N                        opz. 15¬
158700970526                   -  -     (S/N)=N                        opz. 16¬
158800970526                   -  -     (S/N)=N                        opz. 17¬
158900970526                   -  -     (S/N)=N                        opz. 18¬
159000970526                   -  -     (S/N)=N                        opz. 19¬
159100970526                   -  -     (S/N)=N                        opz. 20¬
159200970526                   -  -     (S/N)=N                        opz. 21¬
159300970526                   -  -     (S/N)=N                        opz. 22¬
159400161128                   -  -     (S/N)=N                        opz. 23¬
159500970526                   -  -     (S/N)=N                        opz. 24¬
159600970526** FUD - FUNZIONI
159700970526                   -        (S/N)=N                        funz.01¬
159800970526                   -        (S/N)=N                        funz.02¬
159900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
160000970526                   -        (S/N)=N                        funz.04¬
160100970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
160200970526F06=Inserimento    -        (S/N)=N                        funz.06¬
160300970526                   -        (S/N)=N                        funz.07¬
160400161128                   -        (S/N)=N                        funz.08¬
160500160713                   -        (S/N)=N                        funz.09¬
160600160713                   -        (S/N)=N                        funz.10¬
160700160713                   -        (S/N)=N                        funz.11¬
160800160923F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
160900160713                   -        (S/N)=N                        funz.13¬
161000161121                   -        (S/N)=N                        funz.14¬
161100161128                   -        (S/N)=N                        funz.15¬
161200170427                   -        (S/N)=N                        funz.16¬
161300970526                   -        (S/N)=N                        funz.17¬
161400970526                   -        (S/N)=N                        funz.18¬
161500970627 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
161600970627 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
161700161121F21=Stampa         -        (S/N)=N                        funz.21¬
161800161121                   -        (S/N)=N                        funz.22¬
161900970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
162000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
