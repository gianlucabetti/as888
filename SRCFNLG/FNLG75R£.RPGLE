000100930203     H DECEDIT('0,') DATEDIT(*DMY.)
000200050224     H* Fnlg75R *----------------------------------------------------*
000300930203     H*                                                              *
000400930203     H*    - STAMPA ELENCO GIACENZE CHIUSE PER CLIENTE  -BATCH-      *
000500930203     H*                                                              *
000600930203     H*--------------------------------------------------------------*
000700940224     H*  01 = ON --> SECONDO LIVELLO/OFF --> PRIMO LIVELLO           *
000800940224     H*  10/13/67 = CHAIN SU TABEL                                   *
000900940224     H*  20 = CHAIN SU CNACO/CNIND/FLCLS                             *
001000940224     H*  21 = CLIENTI                                                *
001100940224     H*  22 = CHAIN SU FLBLP25C                                      *
001200940224     H*  23 = CHAIN SU AZORG                                         *
001300940224     H*  24 = CLIENTI ESTERO                                         *
001400940224     H*  25 = CLIENTI ITALIA                                         *
001500940224     H*  36 = LOKUP SU £7                                            *
001600940224     H*  60 = CALL A QCDMEXEC                                        *
001700940224     H*  70 = INDICATORE DI COMODO                                   *
001800940224     H*  88 = PRIMA VOLTA                                            *
001900940224     H*  90 = STAMPA TESTATA                                         *
002000940224     H*--------------------------------------------------------------*
002100050321     FTIGCP07L  UF   E           K DISK    commit
002200050224     FTIGNP01L  IF   E           K DISK
002300930203     FAZORG01L  IF   E           K DISK
002400930203     FTABEL00F  IF   E           K DISK
002500941018     FFNBLP01L  IF   E           K DISK
002600930203     FCNACO00F  IF   E           K DISK
002700050224     FTIGAP01L  IF   E           K DISK
002800930203     FCNIND00F  IF   E           K DISK
002900950227     FFNCLS01L  IF   E           K DISK
003000990730     FTITGC01L  IF   E           K DISK
003100041026     FLETGIC    O    F  132        PRINTER usropn
003200941018     D*--------------------------------------------------------------*
003300020305     E*  ELENCO FILIALI
003400930203     D FIL£7           S              3    DIM(29)
003500020305     E*  SKI X CITTA' DATA
003600941018     D DATX            S              1    DIM(32)
003700020305     E*  COD. NAZIONE
003800020305     D*CNZ             S              3    DIM(500)
003900020305     E*  FLAG ITALIA/ESTERO
004000020305     D*ITA             S              1    DIM(500)
004100020305     E*  COD. TAB.2H
004200960213     D C2H             S              1    DIM(100)
004300020305     E*  FLAG TAB 2H
004400960213     D D2H             S             69    DIM(100)
004500041026     E*  QCAEXEC
004600041026     D DLT1            S             49    DIM(1) CTDATA PERRCD(1)
004700941018     D*--------------------------------------------------------------*
004800950227     D DS15          E DS
004900930212     D DS3K          E DS
005000930205     D DS2C          E DS
005100950321     D DS2A          E DS
005200960213     D DS2H          E DS
005300000202     D OG143         E DS
005400160310     D trul27ds1     e ds
005500050608     D fnlg08ds      e ds
005600020821     d trul06ds      e ds
005700020821     d  lin                    1     90  0 dim(30)
005800041028     D trul44ds      E DS
005900050902     D trtcm1ds      E DS
006000990628     D TRUL43        E DS                  EXTNAME(TRUL43DS)
006100960213     D TRUL42        E DS                  EXTNAME(TRUL42DS)
006200960213     D FNLG74        E DS                  EXTNAME(FNLG74DS)
006300960123     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
006400930203     D                 DS
006500930203     D  GCPSCM                 1      7  0
006600930203     D  §8899                  4      7  0
006700930203     D TCUDS           DS
006800930203     D  F3                     3      3
006900900423     D  F4                     4      4
007000900423     D  F56                    5      6
007100900502     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
007200900423     D  TCU                  398    697
007300930203     D                                     DIM(50)
007400900423     D  KCU                  698    847P 0
007500930203     D                                     DIM(50)
007600930203     D                                     PACKEVEN
007700900502     D CNCR80        E DS
007800900608     D CNCR42        E DS
007900900418     D KPJBA         E DS
008000020930     d savkpjbu        s                   like(kpjbu)
008100041028      **
008200041028     d Aperto_PRTF     s              1
008300041028      **
008400940217     D*-----PROFILO COLLEGATO
008500941018     D WLBDA8          DS
008600941018     D  G02DAT                 1      8  0
008700941018     D  G02INV                 9     16  0
008800941018     D  G02ERR                17     17
008900941018     D  G02TGI                18     22  0
009000150514     D fnlv59ds      E DS
009100960213     D                 DS
009200960213     D  WTPM                   1      1
009300960213     D  WFAX                   2      2
009400000913     D  WVAG                   3      3
009500960213     D  UNI                    1     69
009600041110      *
009700041110     d out70           s             71
009800041110     d out71           s             71
009900041110     d out72           s             71
010000041110      *
010100041111     D FNLV55DS      E DS
010200900423     C*---------------------------------------------------------------*
010300020917     c                   clear                   trul06ds
010400020821     c                   if        simpou = simfel
010500020821     C* Cerco le filiali gestite nell'area
010600020821     c                   eval      d06cod = '£1'
010700020821     c                   movel     simfel        d06key
010800020930     c                   movel     kpjbu         savkpjbu
010900020821     c                   movel     trul06ds      kpjbu
011000020821     c                   call      'TRUL06R'
011100020821     c                   parm                    kpjba
011200020821     c                   movel     kpjbu         trul06ds
011300020930     c                   movel     savkpjbu      kpjbu
011400020821     c                   else
011500020821      *se è un secondo livello forzo solo quello in skiera
011600020821     c                   move      simpou        lin(1)
011700020821     c                   endif
011800020821
011900020821     c     kgcp          klist
012000020821     c                   kfld                    ksimpou           3 0
012100020821     c                   kfld                    codcli
012200041026
012300020821      *devo eseguire il loop per ogni filiale se sono il primo livello a
012400020821      * causa dei fax che vengono inviati solo dal primo
0125000410261     **
012600020821     c                   do        30            i                 3 0
012700041026      **
012800020821     c                   if        lin(i) > 0
012900020821     c                   z-add     lin(i)        ksimpou
013000020821     c                   else
013100020821     c                   leave
013200020821     c                   end
013300041026      **
013400060217     c                   if        d74cod = 0
013500060317     c     ksimpou       setll     tigcp07l
013600060217     c                   else
013700060217     C     kgcp          SETLL     tigcp07L                           10
013800060217     c                   end
0139000410262     **
014000060621     C                   DO        *hival
014100041026
014200020821     C     CODCLI        IFNE      0
014300050224     C     kgcp          READE     TIGCP000                             1510
014400060621     c                   if        *in15
014500060621     C     kgcp          reade(n)  TIGCP000                             1510
014600060621     c                   iter
014700060621     c                   endif
014800060621      *
014900020821     C                   ELSE
015000050224     C     ksimpou       READe     TIGCP000                             1510
015100060621     c                   if        *in15
015200060621     C     ksimpou       reade(n)  TIGCP000                             1510
015300060621     c                   iter
015400060621     c                   endif
015500020821     C                   END
0156000410263    C**
015700060621     C   10              leave
015800041026     C*
015900950227     C** Se il tipo comunicazione fine giacenza è a blanks lo valorizzo
016000950227     C     GCPTFG        IFEQ      ' '
016100950227     C     GCPFAS        ANDEQ     50
016200950227     C                   EXSR      REPTFG
016300950227     C                   END
016400041026     C*
016500960213     C** Reperisco infromazioni sul tipo di comunicazione di chiusura
016600960213     C                   Z-ADD     1             §F
016700960213     C                   CLEAR                   DS2H
016800960213     C     GCPTFG        LOOKUP    C2H(§F)                                32
016900960213     C     *IN32         IFEQ      '1'
017000960213     C                   MOVE      D2H(§F)       UNI
017100960213     C                   MOVE      WTPM          §2HTPM
017200041028      * sul invio con Fax -> 'F' adesso può essere anche E-mail -> 'M'
017300960213     C                   MOVE      WFAX          §2HFAX
017400000913     C                   MOVE      WVAG          §2HVAG
017500960213     C                   END
017600041028      *
017700041028      *  Attenzione: se stiamo trattando solo le E-mail o solo la
017800041028      *   trasmissione dei FAX deve controllare la congruenza
017900041028     c                   move      'SI'          new_giacenza      2
018000041028      *
018100041028      *  in base ai 2 parametri
018200041028      *  Quando deve saltare a nuova giacenza :  S=Salta --> (Iter)
018300041028      *                                          N=OK continua
018400041028      *
018500041028      *                       tipologia comunicazione"2H"
018600041028      *                                 §2Hfax
018700041028      *                           | blk |  F  |  M  |
018800041028      *                D74FAX === +=====+=====+=====+
018900041028      *                  E        |  N  |  N  |  N  |
019000041028      *    da DS       ---------- *-----+-----+-----+
019100041028      *    tipo          N        |  N  |  S  |  S  |
019200041028      *    chiamata    ---------- *-----+-----+-----+
019300041028      *    pgm           F        |  S  |  N  |  S  |
019400041028      *                ---------- *-----+-----+-----+
019500041028      *                  M        |  S  |  S  |  N  |
019600041028      *                ---------- *-----+-----+-----+
019700041028      *
019800041028      * Se il record letto corrisponde ai parametri di richiesta
019900041028      *   non deve saltare ad un'altra giacenza.
020000041028     c                   if        D74fax = 'E' or
020100041029     c                             D74fax = 'N' and §2Hfax = ' ' or
020200041029     c                             D74fax = 'F' and §2Hfax = 'F' or
020300041029     c                             D74fax = 'M' and §2Hfax = 'M'
020400041028     c                   move      'NO'          new_giacenza
020500041028     c                   end
020600041028     C*
020700041028     C*   Ma anche se sto trattando la E-Mail di un cliente generico
020800041028     C*    NON posso inviare l'E-mail ad un 8888 o un 9999.
020900041028     c                   if        D74fax = 'M'
021000041028     C                   move      GCPSCM        codmit            4 0
021100041028     c                   if        codmit = 8888 or
021200041028     c                             codmit = 9999
021300041028     c                   move      'SI'          new_giacenza
021400041028     c                   end
021500041028     c                   end
021600041028     C*
021700041028     C* Deve saltare a leggere una nuova giacenza poichè quella appena
021800041028     C*  letta NON corrisponde ai parametri di filtro passati.
021900041028     c                   if        new_giacenza = 'SI'
022000041028     C*              *----------*
022100041028     c                   iter
022200041028     C*              *----------*
022300041028     c                   end
022400041028     C*
022500960213     C*  CONTROLLO SE GIACENZA APPARTIENE FILIALE IN GESTIONE
022600960213     C*  O SE SONO PRIMO LIVELLO E STO STAMPANDO I FAX
022700041028      *  oppure  se il pgm è richiamato x invio E-mail
022800041028      *    può essere mandato sia da 1° che da 2° Livello
022900960213     C                   MOVE      GCPLNP        §LNP              3
023000960213     C     §LNP          LOOKUP    FIL£7                                  36
023100041028     C*
023200960213     C     *IN36         IFEQ      '0'
023300041028     C     D74FIL        ANDEQ     SIMFEL
023400041028     C     §2HFAX        ANDEQ     'F'
023500041028      **
023600041028     C     *IN36         orEQ      '0'
023700041028     C     §2HFAX        andEQ     'M'
023800041028     C     d74FAX        andEQ     'M'
023900041028      **
024000960213     C                   SETON                                        36
024100960213     C                   END
024200041028      *
024300941018     C** SE LA DATA CHIUSURA DIVERSA
024400930205     C** DAL PERIODO IMPOSTATO O SE CLIENTE 88/99 NON FACCIO NULLA
024500960213     C     §2HTPM        IFEQ      'N'
024600950227     C     D74RIS        ANDEQ     *BLANKS
024700950314     C     GCPDLM        ANDEQ     0
024800950314     C     GCPFAS        ANDEQ     50
024900950227     C                   Z-ADD     DATEU         GCPDLM
025000950227     C                   Z-ADD     60            GCPFAS
025100950227     C                   EXCEPT    AGGIO
025200050321     c                   if        gescomm = 'S'
025300050321     c                   commit
025400050321     c                   endif
025500970109     C                   MOVEL     '3K'          COD
025600970109     C                   MOVEL     *BLANKS       KEY
025700970109     C                   MOVEL     GCPSCM        KEY
025800970109     C     KTAB          CHAIN     TABEL                              67
025900970109     C*
026000970109     C  N67              DO
026100970109     C                   MOVEL     TBLUNI        DS3K
026200041026     C*  E' PREVISTO SCARICO  DELLE CHIUSURE
026300970109     C     §3KGQI        IFEQ      'C'
026400970109     C     §3KGQI        OREQ      'E'
026500970109     C                   EXSR      SCAR
026600970109     C                   END
026700970109     C                   END
026800950227     C                   END
026900041026      **
027000041026      *   A questo punto è tutto OK quindi 36 = *on -> si procede con
027100041028      *    la stampa
0272000410264     **
027300970109     C     *IN36         IFEQ      '1'
027400041028      **
0275000410265    C*
027600950227     C     GCPDBG        IFLE      D74DT2
027700930203     C     GCPFAS        ANDEQ     50
027800920522     C     GCPFRG        ANDEQ     0
027900930224     C     GCPDLM        ANDEQ     0
028000941018     C     D74RIS        ANDEQ     *BLANKS
028100960213     C     §2HTPM        ANDEQ     'R'
028200941018     C     GCPDLM        ORGE      D74DT1
028300941018     C     GCPDLM        ANDLE     D74DT2
028400941019     C     GCPFAS        ANDGE     50
028500930224     C     GCPFRG        ANDEQ     0
028600941018     C     D74RIS        ANDEQ     'R'
028700960213     C     §2HTPM        ANDEQ     'R'
028800930215     C*
028900960213     C                   SETOFF                                       14
029000960314     C*  Controllo se esiste un luogo di invio comunicazione di giac.
029100960314     C                   CLEAR                   TRUL43
029200960314     C                   Z-ADD     GCPSCM        D43KSC
029300960314     C                   CALL      'TRUL43R'
029400960314     C                   PARM                    TRUL43
029500960618     C     D43PER        COMP      *BLANKS                            0404
029600970819     C                   MOVEL     D43PER        ATTPER           27
029700041026      **
029800041026      *  Attenzione a come reperire il Luogo x Indirizzo E-mail
029900041028      *   e considerare anche le note (88) !!!!!
030000041026      **
030100041028     C     §2HFAX        IFEQ      'M'
030200041028     C     D74FAX        ANDeq     'M'
030300041028     C                   EXSR      CTRMail
030400041028     C                   END
030500041026     C*
030600960213     C     §2HFAX        IFEQ      'F'
030700960213     C     D74FAX        ANDNE     'N'
030800041028     C     D74FAX        andne     'M'
030900960213     C                   EXSR      CTRFAX
031000041110     c                   exsr      repMITT
031100960213     C                   END
031200041026      **
031300041026      ** Ora deve prevedere anche la E-mail oltre che il FAX
0314000410266    C*
031500960213     C     §2HFAX        IFEQ      *BLANKS
031600960213     C     D74FAX        ANDNE     'F'
031700041028     C     D74FAX        ANDNE     'M'
031800041028     C* x FAX
031900960213     C     §2HFAX        OREQ      'F'
032000960213     C     D74FAX        ANDNE     'N'
032100041028     C     D74FAX        andne     'M'
032200041028     C* anche x mail
032300041028     C     §2hfax        OREQ      'M'
032400041028     C     d74fax        ANDeq     'M'
032500960213     C*
032600920522     C                   EXSR      STAMPA
032700041026     C*
032800930208     C* SE LA DATA STAMPA LETTERA E' UGUALE A 0 LA AGGIORNO
032900930215     C* SE E' PREVISTA LA PREPARAZIONE ARCHIVI DA TRASMETTERE
033000930212     C* AL CLIENTE LA ESEGUO
0331000410267    C*
033200941018     C  N22GCPDLM        IFEQ      0
033300930208     C                   Z-ADD     DATEU         GCPDLM
033400941018     C                   Z-ADD     60            GCPFAS
033500930208     C                   EXCEPT    AGGIO
033600050321     c                   if        gescomm = 'S'
033700050321     c                   commit
033800050321     c                   endif
033900930212     C                   MOVEL     '3K'          COD
034000930212     C                   MOVEL     *BLANKS       KEY
034100930212     C                   MOVEL     GCPSCM        KEY
034200930212     C     KTAB          CHAIN     TABEL                              67
034300950419     C*
034400930212     C  N67              DO
034500930212     C                   MOVEL     TBLUNI        DS3K
034600041026     C*  E' PREVISTO SCARICO   DELLE CHIUSURE
034700930212     C     §3KGQI        IFEQ      'C'
034800930212     C     §3KGQI        OREQ      'E'
034900930212     C                   EXSR      SCAR
035000930212     C                   END
035100960213     C                   END
0352000410267-/3-C*
035300930212     C                   END
035400960213     C                   END
035500930208     C                   END
035600930215     C                   END
0357000410262-/1- *
035800041026     C                   ENDdo
035900020821     c                   enddo
036000020821
036100041028     C   90              do
036200041028     C                   EXCEPT    FINE
036300041110     C                   EXCEPT    FINFAX
036400041028      ***
036500041028      ***   Attenzione quando si gestisce la E-mail occorre chiudere
036600041028      ***   qui il file poichè siamo ai saluti.
036700041028      ***
036800041028     c                   if        d74fax = 'M' and Aperto_PRTF = 'S'
036900041028     c                   exsr      Close_PRTF
037000041028     c                   endIF
037100041028      ***
037200041028     c                   endDO
037300920522     C*
037400160310     C                   CLEAR                   TRUL27ds1
037500020306     C                   MOVEL     'C'           I27Tla
037600160310     C                   CALL      'TRUL27R1'
037700160310     C                   PARM                    TRUL27ds1
037800041111      *
037900160310     C                   CLEAR                   fnlv59ds
038000160310     C                   MOVEL     'C'           Ilv59tla
038100160310     C                   CALL      'FNLV59R'
038200160310     C                   PARM                    fnlv59ds
038300160310      *
038400041111     C                   CLEAR                   FNLV55DS
038500041111     C                   MOVEL     'C'           D55TLA
038600041111     C                   CALL      'FNLV55R'
038700041111     C                   PARM                    FNLV55DS
038800041026      ***
038900920522     C                   SETON                                          LR
039000960213     C*------------------------------------------------------*
039100960213     C*  Posizionam. su anagrafica indirizzi x fax
039200960213     C*------------------------------------------------------*
039300960213     C     CTRFAX        BEGSR
039400960213     C*
039500961128    1C     D74RIS        IFNE      'R'
039600960213     C                   MOVE      GCPSCM        CODMIT            4 0
039700961128    2C     CODMIT        IFNE      8888
039800960213     C     CODMIT        ANDNE     9999
039900960213     C     KACO          CHAIN     CNIND000                           26
040000960213      *  Se invio tramite fax richiamo pgm x controllo validità
040100961128    3C     §2HFAX        IFEQ      'F'
040200960213     C                   CLEAR                   TRUL42
040300960213     C                   MOVEL     INDTLF        D42FAX
040400961128    4C     D43TRV        IFEQ      'S'
040500960314     C                   MOVEL     D43FAX        D42FAX
040600961128    4C                   END
040700960213     C                   CALL      'TRUL42R'
040800960213     C                   PARM                    TRUL42
040900961128    4C     D42ERR        IFEQ      '1'
041000960213     C                   MOVEL     *BLANKS       §2HFAX            1
041100960213     C                   Z-ADD     1             §F
041200960213     C                   MOVE      §2HTPM        WTPM
041300960213     C                   MOVE      §2HFAX        WFAX
041400960213     C     UNI           LOOKUP    D2H(§F)                                32
041500961128    5C     *IN32         IFEQ      '1'
041600960213     C                   MOVEL     C2H(§F)       GCPTFG
041700960213     C                   EXCEPT    AGG1
041800050321     c                   if        gescomm = 'S'
041900050321     c                   commit
042000050321     c                   endif
042100961128    5C                   END
042200961128   X4C                   ELSE
042300960213     C                   SETON                                        14
042400960213     C                   MOVEL     INDTLF        STPFAX           16
042500961128    5C     D43TRV        IFEQ      'S'
042600960314     C                   MOVEL     D43FAX        STPFAX
042700961128    5C                   END
042800961128    4C                   END
042900961128    3C                   END
043000961128   X2C                   ELSE
043100961128    3C     §2HFAX        IFEQ      'F'
043200960213     C                   MOVEL     *BLANKS       §2HFAX
043300960213     C                   Z-ADD     1             §F
043400960213     C                   MOVE      §2HTPM        WTPM
043500960213     C                   MOVE      §2HFAX        WFAX
043600960213     C     UNI           LOOKUP    D2H(§F)                                32
043700961128    4C     *IN32         IFEQ      '1'
043800960213     C                   MOVEL     C2H(§F)       GCPTFG
043900960213     C                   EXCEPT    AGG1
044000050321     c                   if        gescomm = 'S'
044100050321     c                   commit
044200050321     c                   endif
044300961128    4C                   END
044400961128    3C                   END
044500961128    2C                   END
044600960213     C*
044700961128   X1C                   ELSE
044800960213     C                   MOVE      GCPSCM        CODMIT            4 0
044900961128    2C     CODMIT        IFNE      8888
045000960213     C     CODMIT        ANDNE     9999
045100960213     C     §2HFAX        ANDEQ     'F'
045200960213     C     KACO          CHAIN     CNIND000                           26
045300960213     C                   MOVEL     INDTLF        STPFAX           16
045400961128    3C     D43TRV        IFEQ      'S'
045500960314     C                   MOVEL     D43FAX        STPFAX
045600961128    3C                   END
045700961128    2C                   END
045800961128    1C                   END
045900960213     C*
046000960213     C                   ENDSR
046100930212     C*----------------------------------------------------------------*
046200041028     C*  Controllo dati x mail
046300041028     C*----------------------------------------------------------------*
046400041028     C     CTRMail       BEGSR
046500941018     C*
046600041028     C*
046700041028     C*
046800041028     C                   ENDSR
046900041028     C*----------------------------------------------------------------*
047000041028     C*  Stampa
047100041028     C*----------------------------------------------------------------*
047200041028     C     STAMPA        BEGSR
047300041028     C*
047400041028      **  e deve gestire apertura a livello di testata con OVRPRTF
047500041028      **   particolare tramite il TRUL44R
047600041028     C*
047700950620     C                   MOVEL     *BLANKS       ACORAG
047800950620     C                   MOVEL     *BLANKS       INDVIA
047900950620     C                   MOVEL     *BLANKS       INDCIT
048000950620     C                   MOVEL     *BLANKS       INDPRV
048100950620     C                   MOVEL     *BLANKS       INDSTA
048200041028     C*
048300930209     C* DECODIFICO IL CLIENTE A CAMBIO CODICE
048400930209     C     GCPSCM        IFNE      SAVSCM
048500960213     C     GCPTFG        ORNE      SAVTFG
048600950103     C     §8899         OREQ      8888
048700950103     C     §8899         OREQ      9999
048800041026      ***
048900950103     C* LA PRIMA VOLTA SALTO I SALUTI
049000950103     C   88
049100041028     CANN22              EXCEPT    FINE
049200041110     C   88
049300041110     CANN22              EXCEPT    FINFAX
049400941018     C  N88              SETON                                        88
049500041028      ***   Attenzione quando si gestisce la E-mail occorre chiudere
049600041028      ***   qui il file poichè siamo ai saluti.
049700041028      ***
049800041028      ** solo x E-mail
049900041028     c                   exsr      OVRPRTF_x_Mail
050000041028      ***
050100960314     C     D43TRV        IFNE      'S'
050200960314     C*
050300950620     C     §8899         IFNE      8888
050400950620     C     §8899         ANDNE     9999
050500941018     C     KACO          CHAIN     CNACO000                           20
050600960618     C                   MOVEL     ACORAG        WRAG             38
050700941018     C  N20              MOVEL     ACORAG        PRAG             30
050800941018     C   20              MOVEL     *ALL'?'       PRAG
050900941018     C  N20KACO          CHAIN     CNIND000                           20
051000941019     C     *IN20         IFEQ      '0'
051100960619     C                   MOVEL     INDVIA        WVIA
051200960619     C                   MOVEL     INDPRV        WPRV
051300960619     C                   MOVEL     INDCIT        WCIT
051400960619     C                   MOVEL     INDSTA        WSTA
051500941019     C                   MOVEL     *BLANKS       CAP9              9
051600941019     C                   MOVEL     INDCAP        CAP9
051700941019     C     INDCAE        IFNE      *BLANKS
051800941019     C     INDCAE        ANDNE     *ZEROS
051900941019     C                   MOVEL     INDCAE        CAP9
052000941019     C                   END
052100941019     C                   END
052200950620     C*
052300950620     C                   ELSE
052400050224     C     KBLP          CHAIN     TIGAP01L                           20
052500950620     C     *IN20         IFEQ      '0'
052600950620     C                   MOVEL     GAPRSM        PRAG             30
052700960618     C                   MOVEL     GAPRSM        WRAG
052800950620     C                   MOVEL     GAPCAM        CAP9
052900960619     C                   MOVEL     GAPINM        WVIA
053000960619     C                   MOVEL     GAPPRM        WPRV
053100960619     C                   MOVEL     GAPLOM        WCIT
053200960619     C                   MOVEL     GAPNZM        WSTA
053300950620     C                   ELSE
053400950620     C     KBLP          CHAIN     FNBLP01L                           22
053500960618     C                   MOVEL     BLPRSM        WRAG
053600960619     C                   MOVEL     BLPINM        WVIA
053700960619     C                   MOVEL     BLPLOM        WCIT
053800960619     C                   MOVEL     BLPPRM        WPRV
053900950620     C                   MOVEL     BLPCAM        CAP9
054000960619     C                   MOVEL     BLPNZM        WSTA
054100950620     C                   END
054200950620     C*
054300950620     C                   END
054400960314     C*  Controllo se devo reimpostare i dati dal luogo
054500960314     C                   ELSE
054600960618     C                   MOVEL     D43RAG        WRAG
054700960619     C                   MOVEL     D43IND        WVIA
054800960619     C                   MOVEL     D43LOC        WCIT
054900960619     C                   MOVEL     D43PRO        WPRV
055000960314     C                   MOVEL     D43CAP        CAP9
055100960619     C                   MOVEL     D43NAZ        WSTA
055200960314     C                   END
055300950620     C                   Z-ADD     GCPSCM        SAVSCM            7 0
055400960213     C                   MOVEL     GCPTFG        SAVTFG            1
055500941018     C                   SETOFF                                       90
055600941018     C                   Z-ADD     0             PAGE
055700941018     C                   END
055800941018     C*
055900900423     C** RICERCO DESCRIZIONE DELLA FILIALE DI PARTENZA
056000950103     C     KBLP          CHAIN     FNBLP01L                           22
056100960123     C                   CLEAR                   FNLSN0
056200960123     C                   Z-ADD     BLPAAS        DN0AAS
056300960123     C                   Z-ADD     BLPLNP        DN0LNP
056400960123     C                   Z-ADD     BLPNRS        DN0NRS
056500960123     C                   Z-ADD     BLPNSP        DN0NSP
056600960123     C                   MOVEL     *BLANKS       DN0TRC
056700960125     C                   Z-ADD     GCPDUR        DN0DAG
056800960123     C                   MOVEL     KPJBU         WSVKPJ          256
056900960123     C                   MOVEL     FNLSN0        KPJBU
057000960123     C                   CALL      'FNLSN0R'
057100960123     C                   PARM                    KPJBA
057200960123     C                   MOVEL     KPJBU         FNLSN0
057300960123     C                   MOVEL     WSVKPJ        KPJBU
057400960123     C* Destinatario
057500960123     C     DN0TRV        IFEQ      'S'
057600960123     C                   MOVEL     DN0RSD        STPRSD
057700960123     C                   MOVEL     DN0LOD        STPLOD
057800960123     C                   MOVEL     DN0PRD        STPPRD
057900960123     C                   MOVEL     DN0CAD        STPCAD
058000960123     C                   MOVEL     DN0NZD        STPNZD
058100960123     C                   ELSE
058200960123     C* DESTINATARIO
058300960123     C                   MOVEL     BLPRSD        STPRSD
058400960123     C                   MOVEL     BLPLOD        STPLOD
058500960123     C                   MOVEL     BLPPRD        STPPRD
058600960123     C                   MOVEL     BLPCAD        STPCAD
058700960123     C                   MOVEL     BLPNZD        STPNZD
058800960123     C                   END
058900950103     C     *IN22         IFEQ      '0'
059000960618     C                   SETON                                        13
059100971113     C                   MOVEL     *BLANKS       ORGFL1
059200900430     C     GCPLNP        CHAIN     AZORG                              23
059300020717     C  N23              MOVEL     ORGDE3        OG143
059400020717     C  N23§OgNTW        ifeq      'FED'
059500020717     C     §OgNTW        oreq      'EEX'
059600020717     C     §OgNTW        oreq      'EUP'
059700971113     C                   SETOFF                                       13
059800971113     C                   END
059900911018     C  N23              DO
060000900423     C                   MOVEL     *BLANKS       DESLNP           20
060100900423     C                   MOVEL     *BLANKS       INDLNP           20
060200900423     C                   Z-ADD     0             CAPLNP            5 0
060300900423     C                   MOVEL     *BLANKS       LOCLNP           20
060400900423     C                   MOVEL     *BLANKS       PROLNP            2
060500900423     C                   MOVEL     *BLANKS       FAXLNP           13
060600900423     C                   MOVEL     *BLANKS       TLXLNP           13
060700900423     C                   MOVEL     *BLANKS       TELLNP           13
060800900423     C                   MOVEL     ORGDES        DESLNP
060900900423     C                   MOVEL     ORGIND        INDLNP
061000900430     C                   Z-ADD     ORGCPF        CAPLNP
061100900423     C                   MOVEL     ORGLOC        LOCLNP
061200900423     C                   MOVEL     ORGPRO        PROLNP
061300900423     C                   MOVEL     ORGTEL        TELLNP
061400900423     C                   MOVEL     ORGFAX        FAXLNP
061500900423     C                   MOVEL     ORGTLX        TLXLNP
061600041111     c  N13              exsr      Hub_estero
061700930225     C                   EXSR      CUCITO
061800900423     C                   END
061900900511     C** RICERCO DESCRIZIONE DEL COD.ELABORATORE
062000960618     C                   SETON                                        13
062100971113     C                   MOVEL     *BLANKS       ORGFL1
062200920522     C     BLPLNP        CHAIN     AZORG                              70
062300020717     C  N70              MOVEL     ORGDE3        OG143
062400020717     C  N70§OgNTW        ifeq      'FED'
062500020717     C     §OgNTW        oreq      'EEX'
062600020717     C     §OgNTW        oreq      'EUP'
062700971113     C                   SETOFF                                       13
062800971113     C                   END
062900900511     C                   MOVEL     *BLANKS       DESELA           20
063000900511     C                   MOVEL     *BLANKS       INDELA           20
063100900511     C                   Z-ADD     0             CAPELA            5 0
063200900511     C                   MOVEL     *BLANKS       LOCELA           20
063300900511     C                   MOVEL     *BLANKS       PROELA            2
063400900511     C  N70              DO
063500900511     C                   MOVEL     ORGDES        DESELA
063600900511     C                   MOVEL     ORGIND        INDELA
063700900511     C                   Z-ADD     ORGCPF        CAPELA
063800900511     C                   MOVEL     ORGLOC        LOCELA
063900900511     C                   MOVEL     ORGPRO        PROELA
064000900511     C                   END
064100900507     C** RIBALTAMENTO DATE
064200900507     C                   EXSR      RIBDAT
064300900423     C*
064400920522     C     *IN90         IFEQ      '0'
064500041029      **
064600041029     c                   if        d74fax <> 'M'
064700961114     C   14              EXCEPT    TESTA
064800961114     C  N14              EXCEPT    TESTAN
064900930211     C                   EXCEPT    TESTA1
065000930211     C                   EXCEPT    TESTA2
065100930211     C                   Z-ADD     24            CONT
065200041029     c                   else
065300041029     C* se x e-mail
065400041029     C   14              EXCEPT    TESMA
065500041029     C  N14              EXCEPT    TESMAN
065600041029     C                   EXCEPT    TESMA1
065700041029     C                   EXCEPT    TESTA2
065800041029     C                   Z-ADD     15            CONT
065900041029     c                   end
066000041029      **
066100930211     C                   SETON                                        90
066200930211     C                   END
066300041026      ***
066400930211     C     CONT          IFGE      56
066500041029     c                   if        d74fax <> 'M'
066600961114     C   14              EXCEPT    TESTA
066700961114     C  N14              EXCEPT    TESTAN
066800930211     C                   EXCEPT    TESTA2
066900930211     C                   Z-ADD     20            CONT
067000041029     c                   else
067100041029     C* se x e-mail non deve rieseguire la testata x saltopagina
067200041029     c                   end
067300920522     C                   END
067400041026      ***
067500950103     C** RICERCO DESCRIZIONE CODICE CHIUSURA GIACENZA SULLA TABELLA
067600950103     C                   MOVEL     '2C'          COD
067700950103     C                   MOVEL     *BLANKS       KEY
067800950103     C                   MOVEL     GCPCFG        KEY
067900950103     C     KTAB          CHAIN     TABEL                              70
068000950103     C  N70              MOVEL     TBLUNI        DS2C
068100060505     C                   MOVEL     §2CDES        PDES             40
068200060505      * totale spese
068300060505     c                   z-add     gcpsgs        totspe           11 3
068400060505     c                   add       gcpsgr        totspe
068500060505     c                   add       gcpsgd        totspe
068600060505     c                   add       gcpsga        totspe
068700041026      ***
068800930203     C                   EXCEPT    RIGA
068900930211     C                   ADD       4             CONT
069000041026      ***
069100930203     C                   END
069200950103     C*
069300930203     C                   ENDSR
069400041028     C*----------------------------------------------------------------*
069500041028     C*  Chiusura PRTF  x mail
069600041028     C*----------------------------------------------------------------*
069700041028     C     Close_PRTF    BEGSR
069800041028     C*
069900041028     C                   close     LETGIC
070000041028     c                   eval      Aperto_PRTF = 'N'
070100041028     C*  DLTOVR
070200050609     c                   z-add     50            lung
070300050609     C                   MOVEL     *BLANKS       COMMAN           80
070400041028     C                   MOVEA     dlt1(1)       COMMAN
070500041029     C                   CALL      'QCMDEXC'                            99
070600041028     C                   PARM                    COMMAN
070700050609     C                   PARM                    LUNG             15 5
070800041028     C*
070900041028     C                   ENDSR
071000041028     C*----------------------------------------------------------------*
071100041028     C*  Esegue OVRPRTF x lancio E-mail
071200041028     C*----------------------------------------------------------------*
071300041028     C     OVRPRTF_x_mailBEGSR
071400041028     C*
071500041028     C* Solo x E-mail
071600041028     c                   if        d74fax = 'M'
071700041028     C*
071800041028     C*   Prima di tutto chiude il PRTF
071900041028     c                   if        Aperto_PRTF = 'S'
072000041028     c                   exsr      Close_PRTF
072100041028     c                   endIF
072200041028     C*
072300041028     C*  Quindi riapre eseguendo la giusta OVRPRTF x invio coda E-mail
072400041028     C*   con tutti i parametri tramite il TRUL44R
072500041028     C*
072600041028     c                   if        Aperto_PRTF = 'N'
072700041028     c                   clear                   trul44ds
072800050902     c                   clear                   trtcm1ds
072900041028     c                   eval      d44pgm = 'FNLG75R'
073000041028     c                   eval      d44cl  = '005'
073100041028     c                   move      gcpSCM        d44ksc
073200041028     c                   eval      d44apl = 'C'
073300041118     c                   eval      d44nk1 = '0151' + %editc(gcpSCM:'X')
073400041028     c                   eval      d44tnt = '88'
073500041028     c                   eval      d44scope='*JOB'
073600041028     c                   eval      d44ovr = 'S'
073700041028     c                   eval      d44obj = '*OBJM*Comunicazione Chiusura Giac-
073800041029     c                             enza - Cli.: ' +  %editc(gcpSCM:'Z')
073900041028     c                   eval      d44prtf = 'LETGIC'
074000041028     c                   call      'TRUL44R'
074100041028     c                   parm                    trul44ds
074200050902     c                   parm                    trtcm1ds
074300041028      **  Se c'è stato qualche problema
074400041028     c     d44err        ifne      *blank
074500041028     c                   end
074600041028     C**
074700041028     C*   Apre il file dopo OVRPRTF
074800041028     c                   open      LETGIC
074900041028     c                   eval      Aperto_PRTF = 'S'
075000041028     c                   endIF
075100041028     C*
075200041028     c                   endIF
075300041028     C*
075400041028     C                   ENDSR
075500041110     C*------------------------------------------------------*
075600041110     C*  Prepara stringa mittente per invio riferimento per FAX
075700041110     C*------------------------------------------------------*
075800041110     C     repmitt       begsr
075900041110      **
076000041110     c                   setoff                                       17
076100041110     c                   clear                   out70
076200041110     c                   clear                   out71
076300041110     c                   clear                   out72
076400041110      **
076500041110     c                   IF        d74fax = 'F' or d74fax = 'E'
076600041110      **
076700041110      ** Prende i parametri con il TRUL44R
076800041110     c                   clear                   mitt_x_fax       21
076900041110     c                   clear                   trul44ds
077000050902     c                   clear                   trtcm1ds
077100041110     c                   eval      d44pgm = 'FNLG75R'
077200041110     c                   eval      d44cl  = '005'
077300041110     c                   move      gcpSCM        d44ksc
077400041110     c                   eval      d44apl = 'C'
077500041118     c                   eval      d44nk1 = '0151' + %editc(gcpSCM:'X')
077600041110     c                   eval      d44tnt = '88'
077700041110     c                   eval      d44prtf = 'LETGIC'
077800041110     c                   call      'TRUL44R'
077900041110     c                   parm                    trul44ds
078000050902     c                   parm                    trtcm1ds
078100041110      **  Se c'è stato qualche problema
078200041110     c     d44err        cabeq     'E'           salto_mit
078300041110      **
078400041110     c                   seton                                        17
078500041110     c                   eval      mitt_x_fax = D44mit
078600041110      **
078700041110     c                   eval      out70 = 'Per ricevere le prossime comunicazi-
078800041110     c                             oni via e-mail, Vi invitiamo a darne'
078900041110     c                   eval      out71 = 'segnalazione a: '+
079000041110     c                             %trim(mitt_x_fax) +
079100041110     c                             ' indicando i seguenti dati: '
079200041110     c                   eval      out72 = 'Codice cliente ' +
079300041110     c                             %editc(D44ksc:'Z') +
079400041110     c                             ' e-mail: __________________________________'
079500041110     c                   endIF
079600041110      **
079700041110     C     salto_mit     endsr
079800041028     C*----------------------------------------------------------------*
079900041028     C*  Scarica su FNVAG
080000041028     C*----------------------------------------------------------------*
080100930212     C     SCAR          BEGSR
080200050608     C**----------------------
080300050608     c                   clear                   fnlg08ds
080400050608     c                   eval      D08AGC= gcpagc
080500050608     c                   eval      D08FGC= gcpfgc
080600050608     c                   eval      D08NGC= gcpngc
080700050608     c                   eval      D08uni= §3kgks
080800050608     c                   eval      D08GSD= §3kGSD
080900050608     c                   eval      D08pgm= 'FNLG75R'
081000050609     c                   eval      D08tpr= 'C'
081100050620     c                   eval      D08commit= 'S'
081200050608     c                   movel(p)  fnlg08ds      kpjbu
081300050608     c                   call      'FNLG08R'                            28
081400050608     c                   parm                    kpjba
081500050608     c                   movel     kpjbu         fnlg08ds
081600050608     C*
081700930212     C                   ENDSR
081800930212     C***------------------------------------------------***
081900930203     C     RIBDAT        BEGSR
082000930203     C**----------------------
082100930203     C** RIBALTAMENTO DATE
082200930203     C**----------------------
082300900507     C** DATA SPEDIZIONE
082400941018     C                   MOVEL     BLPAAS        G02INV
082500941018     C                   MOVE      BLPMGS        G02INV
082600941018     C                   Z-ADD     0             G02DAT
082700941018     C                   MOVEL     '3'           G02ERR
082800941018     C                   CALL      'XSRDA8'
082900941018     C                   PARM                    WLBDA8
083000941018     C                   Z-ADD     G02DAT        DATSPE            8 0
083100930203     C** DATA CHIUSURA GIACENZA
083200941018     C                   Z-ADD     GCPDCG        G02INV
083300941018     C                   Z-ADD     0             G02DAT
083400941018     C                   MOVEL     '3'           G02ERR
083500941018     C                   CALL      'XSRDA8'
083600941018     C                   PARM                    WLBDA8
083700941018     C                   Z-ADD     G02DAT        DATDCG            8 0
083800930203     C                   ENDSR
083900930225     C***------------------------------------------------***
084000930225     C     CUCITO        BEGSR
084100930225     C                   MOVEA     *BLANKS       DATX
084200961001     C     '  '          SCAN      ORGDES        W                 2 0
084300950206     C     W             IFEQ      0
084400950206     C                   Z-ADD     21            W
084500950206     C                   END
084600961001     C                   MOVEA     ORGDES        DATX
084700930225     C                   MOVEL     ','           DATX(W)
084800930225     C                   ADD       2             W
084900930225     C     2             SUBST     STRD          APPO              2
085000930225     C                   MOVEA     APPO          DATX(W)
085100930225     C                   ADD       2             W
085200930225     C                   MOVEA     '/'           DATX(W)
085300930225     C                   ADD       1             W
085400930225     C     2             SUBST     STRD:3        APPO
085500930225     C                   MOVEA     APPO          DATX(W)
085600930225     C                   ADD       2             W
085700930225     C                   MOVEA     '/'           DATX(W)
085800930225     C                   ADD       1             W
085900941018     C     4             SUBST     STRD:5        APPO4             4
086000941018     C                   MOVEA     APPO4         DATX(W)
086100930225     C                   MOVEA     DATX          DATAX            30
086200930225     C                   ENDSR
086300950227     C*---------------------------------------------------------------*
086400950227     C*   R E P T F G    Reperisco codice fine giacenza               *
086500950227     C*---------------------------------------------------------------*
086600950227     C     REPTFG        BEGSR
086700950227     C*
086800950227     C                   MOVEL     'R'           GCPTFG
086900950227     C     KBLP          CHAIN     FNBLP01L                           16
087000950227     C                   MOVEL     ' '           WTROVA            1
087100950227     C                   MOVE      GCPSCM        KKSC              7 0
087200950227     C                   MOVE      KKSC          §QUATR            4 0
087300020305     C*                  Z-ADD     1             Y                 3 0
087400020305     C*    BLPNZD        LOOKUP    CNZ(Y)                                 32
087500950227     C  N16§QUATR        IFNE      *ALL'8'
087600950227     C     §QUATR        ANDNE     *ALL'9'
087700950227     C     §QUATR        ANDNE     *ZEROS
087800020305     C                   Z-ADD     BLPMGS        DATDSP            8 0
087900020305     C                   MOVEL     BLPAAS        DATDSP
088000160310     C*
088100160310     C                   CLEAR                   TRUL27ds1
088200021118     C                   MOVE      BLPctr        I27ctb
088300020305     C                   MOVEL     BLPTSP        I27TSP
088400020305     C                   MOVE      BLPLNA        I27LNA
088500020305     C                   MOVE      BLPLNP        I27LNP
088600020305     C                   MOVE      gcpscm        I27CLI
088700160310     C                   CALL      'TRUL27R1'
088800160310     C                   PARM                    TRUL27ds1
088900150514     C                   CLEAR                   fnlv59ds
089000150514     C                   MOVEL     ' '           ilv59TBO
089100150514     C                   Z-ADD     DATDSP        ilv59DSP
089200150514     C                   MOVE      BLPCTR        ilv59CTR
089300150514     C                   MOVE      KKSC          ilv59KSC
089400150514     C                   MOVE      KCI           ilv59KCI
089500150514     C                   MOVEL     o27fie        ilv59FIE
089600150514     C                   MOVEL     BLPTSP        ilv59TSP
089700150514     C                   MOVEL     'S'           ilv59CON
089800150514     c                   eval      ilv59ta2='B'
089900150514     c                   eval      ilv59blo='S'
090000150514     C                   CALL      'FNLV59R'
090100150514     C                   PARM                    fnlv59ds
090200950227     C*
090300150514     C     olv59ERR      IFEQ      *BLANKS
090400150514     C     olv59tks      ANDEQ     'S'
090500950227     C*  Se ho trovato tariffa in vigore mi posiziono sul file
090600950227     C*  delle tariffe giacenze con il proggressivo giusto
090700150514     C                   MOVE      olv59PRG      KPRG
090800150514     C                   MOVE      ilv59KSC      KKSC
090900150514     C                   MOVE      olv59CTR      KCTR
091000990730     C     KTGC          CHAIN     TITGC01L                           31
091100950227     C     *IN31         IFEQ      '0'
091200950227     C     TGCTFG        ANDNE     *BLANKS
091300950227     C                   MOVE      TGCTFG        GCPTFG
091400950227     C                   MOVEL     'S'           WTROVA
091500950227     C                   END
091600950227     C                   END
091700950227     C                   END
091800950227     C*  Se non ho trovato tipo comunicazione mitt. su tariffe
091900950227     C*  lo cerco sull'anagrafica cliente
092000950227     C     WTROVA        IFNE      'S'
092100950227     C                   MOVEL     *BLANKS       CLSFLO
092200950227     C     KKSC          CHAIN     FNCLS01L                           31
092300950227     C     CLSFLO        IFNE      *BLANKS
092400950227     C                   MOVEL     CLSFLO        GCPTFG
092500950227     C                   END
092600950227     C                   END
092700950227     C*
092800950227     C                   ENDSR
092900941018     C***------------------------------------------------***
093000941018     C     *INZSR        BEGSR
093100941018     C*
093200941018     C     *ENTRY        PLIST
093300941018     C                   PARM                    KPJBA
093400941018     C                   MOVEL     KPJBU         FNLG74
093500050321     c                   move      d74comm       gescomm           1
093600041028     C*
093700041026     C* Apre sempre il PRTF
093800041026     c                   open      LETGIC
093900041028     c                   eval      Aperto_PRTF = 'S'
094000041026     C*
094100941018     C                   Z-ADD     1             CODUT
094200941018     C                   CALL      'X§PARUT'
094300941018     C                   PARM                    UTEDSE
094400941018     C*--- RICERCA CAPOCONTI------------------------------------------*
094500941018     C                   DO        50            X                 2 0
094600941018     C                   MOVE      TCU(X)        TCUDS
094700941018     C     F56           IFEQ      'CG'
094800941018     C     F4            ANDEQ     '1'
094900941018     C     F3            ANDEQ     '0'
095000941018     C                   Z-ADD     KCU(X)        KCI               4 0
095100941018     C                   END
095200941018     C                   END
095300941018     C*---------------------------------------------------------------*
095400941018     C                   MOVEL     REC80         CNCR80
095500941018     C                   MOVEL     REC42         CNCR42
095600941018     C                   MOVEL     *BLANKS       RAGSOC
095700941018     C                   MOVEL     SIMRGS        RAGSOC           30
095800110506     C                   MOVEL     simrgs        RSUT             20
095900941018     C*
096000941018     C* GUARDO CHE UTENTE HA FATTO LA RICHIESTA
096100020730     C     SimTpP        COMP      '2'                                    01
096200020730     C                   MOVE      SimPOU        FLE               3
096300941018     C*
096400941018     C* VERIFICO SE LA FILIALE COLLEGATA NE GESTISCE ALTRE
096500941018     C                   MOVEA     *ZEROS        FIL£7
096600941018     C                   MOVEL     *BLANKS       KEY
096700970109     C                   MOVEL     D74FIL        KEY
096800941018     C                   MOVEL     '£7'          COD
096900941018     C     KTAB          CHAIN     TABEL                              13
097000941018     C  N13              MOVEA     TBLUNI        FIL£7
097100970109     C   13              MOVE      D74FIL        FIL£7
097200950227     C***
097300950227     C     KTGC          KLIST
097400950227     C                   KFLD                    KKSC
097500950227     C                   KFLD                    KCTR
097600950227     C                   KFLD                    KPRG
097700941018     C** TABEL
097800960213     C     KTAB          KLIST
097900941018     C                   KFLD                    CODUT
098000941018     C                   KFLD                    COD               2
098100941018     C                   KFLD                    KEY               8
098200950227     C*
098300950227     C     KTABK         KLIST
098400950227     C                   KFLD                    CODUT
098500950227     C                   KFLD                    COD               2
098600941018     C* CNACO
098700941018     C     KACO          KLIST
098800941018     C                   KFLD                    CODUT
098900941018     C                   KFLD                    KCI
099000990628     C                   KFLD                    GCPSCM
099100941018     C* FLBLP
099200941018     C     KBLP          KLIST
099300941018     C                   KFLD                    GCPAAS
099400941018     C                   KFLD                    GCPLNP
099500941018     C                   KFLD                    GCPNRS
099600941018     C                   KFLD                    GCPNSP
099700941018     C*
099800941018     C     KGNP          KLIST
099900941018     C                   KFLD                    KAGC
100000941018     C                   KFLD                    KFGC
100100941018     C                   KFLD                    KNGC
100200941018     C                   KFLD                    KFRG
100300941018     C                   KFLD                    KFAS
100400941018     C                   KFLD                    KNPR
100500941018     C*---------------------------------------------------------------*
100600941018     C*  LIKE DEFN
100700960123     C     *LIKE         DEFINE    BLPRSD        STPRSD
100800960123     C     *LIKE         DEFINE    BLPLOD        STPLOD
100900960123     C     *LIKE         DEFINE    BLPPRD        STPPRD
101000960123     C     *LIKE         DEFINE    BLPCAD        STPCAD
101100960123     C     *LIKE         DEFINE    BLPNZD        STPNZD
101200960123     C     *LIKE         DEFINE    GNPAGC        KAGC
101300941018     C     *LIKE         DEFINE    GNPFGC        KFGC
101400941018     C     *LIKE         DEFINE    GNPNGC        KNGC
101500941018     C     *LIKE         DEFINE    GNPFRG        KFRG
101600941018     C     *LIKE         DEFINE    GNPFAS        KFAS
101700941018     C     *LIKE         DEFINE    GNPNPR        KNPR
101800950227     C     *LIKE         DEFINE    TGCCTR        KCTR
101900950227     C     *LIKE         DEFINE    TGCPRG        KPRG
102000960619     C     *LIKE         DEFINE    INDVIA        WVIA
102100960619     C     *LIKE         DEFINE    INDCIT        WCIT
102200960619     C     *LIKE         DEFINE    INDPRV        WPRV
102300960619     C     *LIKE         DEFINE    INDSTA        WSTA
102400941018     C*----------------------------------------------------*
102500960213     C* Carico tipi comunicazione fine giacenza
102600960213     C                   Z-ADD     0             §F                3 0
102700960213     C                   MOVEL     '2H'          COD
102800960213     C     KTABK         CHAIN     TABEL                              21
102900960213     C     *IN21         DOWEQ     '0'
103000960213     C     §F            ANDLE     100
103100960213     C     TBLFLG        IFEQ      *BLANK
103200960213     C                   ADD       1             §F
103300960213     C                   MOVEL     TBLKEY        C2H(§F)
103400960213     C                   MOVE      TBLUNI        D2H(§F)
103500960213     C                   END
103600960213     C     KTABK         READE     TABEL                                  21
103700960213     C                   END
103800941018     C* GIRO LA DATA DEL GIORNO
103900941018     C                   TIME                    WHHDAT           14 0
104000941018     C                   CLEAR                   WLBDA8
104100941018     C                   MOVE      WHHDAT        G02DAT
104200941018     C                   CALL      'XSRDA8'
104300941018     C                   PARM                    WLBDA8
104400941018     C                   Z-ADD     G02INV        DATEU             8 0
104500941018     C                   MOVE      WHHDAT        STRD              8
104600941018     C*
104700941018     C                   Z-ADD     0             CONT              3 0
104800941018     C                   Z-ADD     D74COD        CODCLI            7 0
104900941018     C                   Z-ADD     0             CONT
105000941018     C* GIRO LE DATE DEL PERIODO RICHIESTO
105100941018     C                   Z-ADD     D74DT1        G02INV
105200941018     C                   Z-ADD     0             G02DAT
105300941018     C                   MOVEL     '3'           G02ERR
105400941018     C                   CALL      'XSRDA8'
105500941018     C                   PARM                    WLBDA8
105600941018     C                   Z-ADD     G02DAT        §DAL              8 0
105700941018     C*
105800941018     C                   Z-ADD     D74DT2        G02INV
105900941018     C                   Z-ADD     0             G02DAT
106000941018     C                   MOVEL     '3'           G02ERR
106100941018     C                   CALL      'XSRDA8'
106200941018     C                   PARM                    WLBDA8
106300941018     C                   Z-ADD     G02DAT        §AL               8 0
106400941018     C                   Z-ADD     0             SAVSCM
106500041110      **
106600041110      ** riga di separazione in stampa
106700041110     c                   move      *all'-'       lin75            75
106800941018     C*
106900941018     C                   ENDSR
107000930225     C***------------------------------------------------***
107100041111      *  reperisce informazioni Hub di partenza Estero
107200041111     C***------------------------------------------------***
107300041111     C     Hub_Estero    Begsr
107400041111      *
107500041111     C                   CLEAR                   FNLV55DS
107600041111     C                   MOVEL     'P'           D55TPT
107700041111     C                   MOVE      gcpLNP        D55LIN
107800041111     C                   MOVE      gcpLNP        D55LNP
107900041111     C                   MOVE      dateu         d55drf
108000041111     C                   CALL      'FNLV55R'
108100041111     C                   PARM                    FNLV55DS
108200041111     c                   if        d55err = *blanks
108300041111     c     d55tfp        chain     azorg
108400041111     c                   if        %found(azorg01l)
108500041111     c                   movel     orgdes        desLNP
108600041111     c                   movel     orgfax        faxLNP
108700041111     c                   movel     orgtel        telLNP
108800041111     c                   endif
108900041111     c                   endif
109000041111      *
109100041111     C                   ENDSR
109200041111     C***------------------------------------------------***
109300050224     OTIGCP000  E            AGGIO
109400950227     O                       GCPTFG
109500950227     O                       GCPFAS
109600941018     O                       GCPDLM
109700050224     OTIGCP000  E            AGG1
109800960213     O                       GCPTFG
109900990628     C***------------------------------------------------***
110000900608     O** STAMPA LETTERA AL MITTENTE MODULO NUOVO
110100961114     OLETGIC    E            TESTA            05
110200960618     O                                           50 'FAX:'
110300960618     O                       STPFAX              67
110400960618     O          E            TESTA            06
110500120319     o                       gcplnp               7
110600960618     O                                           75 'Pag.:'
110700920525     O                       PAGE          Z   +  1
110800960618     O          E            TESTA            08
110900071018     O                                         +  6 'Filiale di'
111000071018     O*******                                  +  6 'Punto Operativo di'
111100961001     O          E            TESTA            09
111200961001     O                       DESLNP            +  6
111300961001     O          E            TESTA            10
111400960619     O               13      INDLNP            +  6
111500961001     O          E            TESTA            11
111600960619     O               13      CAPLNP            +  6
111700960618     O               13      LOCLNP            +  1
111800960618     O               13      PROLNP            +  1
111900960618     O                                           53 'Spettabile'
112000961001     O          E            TESTA            13
112100960619     O                                         +  6 'FAX'
112200920522     O                       FAXLNP            +  1
112300960618     O                       WRAG                81
112400961001     O          E            TESTA            14
112500960619     O                                         +  6 'TEL'
112600920522     O                       TELLNP            +  1
112700960619     O                       WVIA                73
112800961001     O          E            TESTA            15
112900941019     O                       CAP9                52
113000960619     O                       WCIT              +  1
113100941019     O                                           81 '       '
113200960619     O                       WPRV                77
113300960619     O                       WSTA                81
113400961001     O          E            TESTA            16
113500961001     O                       DATAX             +  6
113600960618     O               04                          53 'Att.ne di '
113700970819     O               04      ATTPER              81
113800961114     O*------------------------------------------------------------------*
113900041029      ** se E-mail
114000041029     O          E            TESMA            01
114100041029     O                                           50 'FAX:'
114200041029     O                       STPFAX              67
114300041029     O          E            TESMA            02
114400041029     O                                           75 'Pag.:'
114500041029     O                       PAGE          Z   +  1
114600041029     O          E            TESMA            04
114700071018     O******                                   +  6 'Punto Operativo di'
114800071018     O                                         +  6 'Filiale di'
114900041029     O          E            TESMA            05
115000041029     O                       DESLNP            +  6
115100041029     O          E            TESMA            06
115200041029     O               13      INDLNP            +  6
115300041029     O          E            TESMA            07
115400041029     O               13      CAPLNP            +  6
115500041029     O               13      LOCLNP            +  1
115600041029     O               13      PROLNP            +  1
115700041029     O                                           53 'Spettabile'
115800041029     O          E            TESMA            09
115900041029     O                                         +  6 'FAX'
116000041029     O                       FAXLNP            +  1
116100041029     O                       WRAG                81
116200041029     O          E            TESMA            10
116300041029     O                                         +  6 'TEL'
116400041029     O                       TELLNP            +  1
116500041029     O                       WVIA                73
116600041029     O          E            TESMA            11
116700041029     O                       CAP9                52
116800041029     O                       WCIT              +  1
116900041029     O                                           81 '       '
117000041029     O                       WPRV                77
117100041029     O                       WSTA                81
117200041029     O          E            TESMA            12
117300041029     O                       DATAX             +  6
117400041029     O               04                          53 'Att.ne di '
117500041029     O               04      ATTPER              81
117600041029     O*------------------------------------------------------------------*
117700961114     O*  TESTATA X NUOVO MODULO
117800961114     O*------------------------------------------------------------------*
117900120328     O          E            TESTAN           04
118000120328     o                       gcplnp               4
118100961114     O          E            TESTAN           10
118200071018     O*****                                    +  6 'Punto Operativo di'
118300120330     O                                         +  0 'Filiale di'
118400961114     O                                           75 'Pag.:'
118500961114     O                       PAGE          Z   +  1
118600961114     O          E            TESTAN           11
118700120330     O                       DESLNP            +  0
118800961114     O                                           53 'Spettabile'
118900961114     O          E            TESTAN           12
119000120330     O               13      INDLNP            +  0
119100961114     O          E            TESTAN           13
119200120330     O               13      CAPLNP            +  0
119300961114     O               13      LOCLNP            +  1
119400961114     O               13      PROLNP            +  1
119500961114     O                       WRAG                81
119600961114     O          E            TESTAN           14
119700120330     O                                         +  0 'FAX'
119800961114     O                       FAXLNP            +  1
119900961114     O                       WVIA                73
120000961114     O          E            TESTAN           15
120100120330     O                                         +  0 'TEL'
120200961114     O                       TELLNP            +  1
120300961114     O                       CAP9                52
120400961114     O                       WCIT              +  1
120500961114     O                                           81 '       '
120600961114     O                       WPRV                77
120700961114     O                       WSTA                81
120800961114     O          E            TESTAN           16
120900120330     O                       DATAX             +  0
121000961114     O               04                          53 'Att.ne di '
121100961114     O               04      ATTPER              74
121200041029     O*------------------------------------------------------------------*
121300041029      ** se E-mail
121400041029     O          E            TESMAN           01
121500071018     O********                                 +  6 'Punto Operativo di'
121600071018     O                                         +  6 'Filiale di'
121700041029     O                                           75 'Pag.:'
121800041029     O                       PAGE          Z   +  1
121900041029     O          E            TESMAN           02
122000041029     O                       DESLNP            +  6
122100041029     O                                           53 'Spettabile'
122200041029     O          E            TESMAN           03
122300041029     O               13      INDLNP            +  6
122400041029     O          E            TESMAN           04
122500041029     O               13      CAPLNP            +  6
122600041029     O               13      LOCLNP            +  1
122700041029     O               13      PROLNP            +  1
122800041029     O                       WRAG                81
122900041029     O          E            TESMAN           05
123000041029     O                                         +  6 'FAX'
123100041029     O                       FAXLNP            +  1
123200041029     O                       WVIA                73
123300041029     O          E            TESMAN           06
123400041029     O                                         +  6 'TEL'
123500041029     O                       TELLNP            +  1
123600041029     O                       CAP9                52
123700041029     O                       WCIT              +  1
123800041029     O                                           81 '       '
123900041029     O                       WPRV                77
124000041029     O                       WSTA                81
124100041029     O          E            TESMAN           07
124200041029     O                       DATAX             +  6
124300041029     O               04                          53 'Att.ne di '
124400041029     O               04      ATTPER              74
124500041029     O*------------------------------------------------------------------*
124600120328     O          E            TESTA1           22
124700960619     O                                         +  6 'Con la presente Vi comun'
124800930211     O                                         +  0 'ichiamo le giacenze riso'
124900930211     O                                         +  0 'lte nel periodo:'
125000120328     O          E            TESTA1           23
125100960619     O                       §DAL              +  6 '  /  /    '
125200930205     O                                         +  0 '-'
125300941018     O                       §AL               +  0 '  /  /    '
125400930211     O                                         +  0 ', anticipandoVi fin d'''
125500941018     O                                         +  0 'ora, che in base agli'
125600120328     O          E            TESTA1           24
125700960619     O                                         +  6 'accordi esistenti, i rel'
125800941018     O                                         +  0 'ativi oneri Vi verranno'
125900941018     O                                         +  0 ' addebitati con la'
126000120328     O          E            TESTA1           25
126100960619     O                                         +  6 'prossima fattura'
126200930211     O                                         +  1 'secondo le tariffe conv'
126300930211     O                                         +  0 'enute.'
126400041029     O*------------------------------------------------------------------*
126500041029      ** se E-mail
126600041029     O          E            TESMA1           10
126700041029     O                                         +  6 'Con la presente Vi comun'
126800041029     O                                         +  0 'ichiamo le giacenze riso'
126900041029     O                                         +  0 'lte nel periodo:'
127000041029     O          E            TESMA1           11
127100041029     O                       §DAL              +  6 '  /  /    '
127200041029     O                                         +  0 '-'
127300041029     O                       §AL               +  0 '  /  /    '
127400041029     O                                         +  0 ', anticipandoVi fin d'''
127500041029     O                                         +  0 'ora, che in base agli'
127600041029     O          E            TESMA1           12
127700041029     O                                         +  6 'accordi esistenti, i rel'
127800041029     O                                         +  0 'ativi oneri Vi verranno'
127900041029     O                                         +  0 ' addebitati con la'
128000041029     O          E            TESMA1           13
128100041029     O                                         +  6 'prossima fattura'
128200041029     O                                         +  1 'secondo le tariffe conv'
128300041029     O                                         +  0 'enute.'
128400041029     O*------------------------------------------------------------------*
128500041029     O*------------------------------------------------------------------*
128600930211     O          E            TESTA2      2  2
128700960619     O                                           11 'Giacenza '
128800941018     O                                         +  1 'Destinatario          '
128900941018     O                                           81 'Spedizione/Rif. Mittente'
129000041029     O*------------------------------------------------------------------*
129100041029     O*------------------------------------------------------------------*
129200920525     O          E            RIGA           0
129300920525     O                       GCPFGC        Z      3
129400920525     O                                         +  0 '/'
129500920525     O                       GCPNGC        Z   +  0
129600960123     O                       STPRSD            +  2
129700941018     O                       GCPAAS        Z   +  2
129800920522     O                       GCPLNP        Z   +  1
129900930203     O                       GCPNRS        Z   +  1
130000930203     O                       GCPNSP        Z   +  1
130100941018     O                       DATSPE            +  2 '  /  /    '
130200930203     O          E            RIGA        1
130300960123     O                       STPCAD              22
130400960123     O                       STPLOD            +  1
130500960123     O                       STPPRD            +  1
130600960123     O                       STPNZD            +  1
130700941018     O                       BLPRMN        Z   +  1
130800930211     O          E            RIGA        1  2
130900060505     O                       PDES                40
131000060801     O                                         +  1 'Spese'
131100060801     O                       totspe        2   +  2
131200060801     O                                         +  4 'Chius: '
131300941018     O                       DATDCG            +  0 '  /  /    '
131400930205     O          E            FINE        2
131500930205     O                                           24 'Distinti Saluti.'
131600920525     O          E            FINE        3
131700110506     O                       rsut                65
131800071018     O          E            FINE        1
131900071018     O                                           55 'Filiale di'
132000071018     O                       DESLNP            +  1
132100041110      **
132200041110      ** invito a trasmettere via e-mail
132300041110     O          E    17      FINFAX      2
132400041110     O                       lin75               78
132500041110     O          E    17      FINFAX      1
132600041110     O                       out70               76
132700041110     O          E    17      FINFAX      1
132800041110     O                       out71               76
132900041110     O          E    17      FINFAX      1
133000041110     O                       out72               76
133100041110     O          E    17      FINFAX      1
133200041110     O                       lin75               78
133300041026**         dlt1
133400041026DLTOVR FILE(LETGIC)
