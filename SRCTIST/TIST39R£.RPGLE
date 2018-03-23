000100000000     H*------------------------------------------------------------------------*
000200050705     H* STAMPA STATISTICA SERVIZI ON-LINE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800050705     FWFSUV02L  IF   E           K DISK
000900050706     FSISTL03L  IF   E           K DISK
001000050705     FAZORG01L  IF   E           K DISK
001100050705     FTABEL00F  IF   E           K DISK
001200050715     FWFST390F  UF A E             DISK
001300050705     FTIST39P   O    E             PRINTER OFLIND(*IN05)
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700000000     D*-------------------
001800000000     D* INTESTAZIONI STAMPA
001900000000     D*-------------------
002000050705     D TES             S             30    DIM(5) CTDATA PERRCD(1)              RIGHE TESTATA
002100000000     D*-------------------
002200000000     D* SCHIERA FILIALI
002300000000     D*-------------------
002400050705     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
002500000000     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
002600000000     D*------------------------------------------------------------------------*
002700000000     D* INPUT
002800000000     D*------------------------------------------------------------------------*
002900000000     D*-------------------
003000000000     D* ARCHITETTURA
003100000000     D*-------------------
003200000000     D KPJBA         E DS
003300000000     D*-------------------
003400000000     D* DS PARAMETRI DI LANCIO PGM
003500000000     D*-------------------
003600000000     D PARAM           DS
003700050705     D  PARDAI                        8  0
003800050705     D  PARDAF                        8  0
003900050705     D  PARCAT                        1
004000050705     D  PARCATFLG                     1
004100050705     D  PARTIP                        2
004200050705     D  PARSPED                       6  0
004300050705     D  PARGIO                        3  0
004400050705     D  PARFILE                       1
004500050706     D  PARLAY                        1
004600050706     D  PARDIV                        1
004700050706     D  PARARE                        3  0
004800050706     D  PARFIL                        3  0
004900050706     D  PARAGE                        7  0
005000060503     D*------------------
005100060503     D* VARIABILI D WRK
005200060503     D*------------------
005300060503     d w0030           s              3  0
005400060503     d xx              s              3  0
005500060503     d yy              s              3  0
005600050715     D*-------------------
005700050715     D* REPERIMENTO DATI UTENTE
005800050715     D*-------------------
005900050715     D TIBS34DS      E DS                                                       *Profili utente
006000050715     D DDATIUTE      E DS                                                       *Dati utente
006100050715     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006200050715     D DUTE01        E DS
006300050715     D TRUL31DS      E DS
006400060503     D**!!! TRUL31DS2     E DS
006500050715     D wAbi            S                   like(UTEAUT)
006600050715     D skDIG           S              1    dim(20)
006700050715     D skARG           S              3    dim(50)
006800050715     D skPOG           S              3    dim(250)
006900000000     C*------------------------------------------------------------------------*
007000000000     C* MAIN LINE
007100000000     C*------------------------------------------------------------------------*
007200050705     C*
007300050705     C* POSIZIONAMENTO SUL FILE PRINCIPALE E PRIMA LETTURA
007400050705     C     *LOVAL        SETLL     WFSUV02L                           99
007500050705     C                   EXSR      LETFIL
007600050705     C*
007700050705     C* DOPO LA 1° LETTURA POSSO IMPOSTARE I DATI D TESTATA
007800050705     C                   EXSR      IMPTES
007900000000     C*
008000000000     C* CICLO FINO A FINE FILE DI LAVORO
008100000000DO  1C     FINL00        DOWEQ     '0'                                          --- 1 -->
008200000000     C*
008300000000     C* OPERAZIONI PER NUOVA DIVISIONE
008400000000     C                   EXSR      NEWDIV
008500000000     C*
008600000000     C* CICLO FINO A ROTTURA DI DIVISIONE
008700000000DO  2C     FINL00        DOWEQ     '0'                                          --- 2 -->
008800050705     C     WFSDIV        ANDEQ     DEPDIV
008900000000     C*
009000000000     C* OPERAZIONI PER NUOVA AREA
009100000000     C                   EXSR      NEWARE
009200000000     C*
009300000000     C* CICLO FINO A ROTTURA DI AREA
009400000000DO  3C     FINL00        DOWEQ     '0'                                          --- 3 -->
009500050705     C     WFSDIV        ANDEQ     DEPDIV
009600050705     C     WFSARE        ANDEQ     DEPARE
009700000000     C*
009800000000     C* OPERAZIONI PER NUOVA FILIALE
009900000000     C                   EXSR      NEWFIL
010000000000     C*
010100000000     C* CICLO FINO A ROTTURA DI FILIALE
010200000000DO  4C     FINL00        DOWEQ     '0'                                          --- 4 -->
010300050705     C     WFSDIV        ANDEQ     DEPDIV
010400050705     C     WFSARE        ANDEQ     DEPARE
010500050705     C     WFSFIL        ANDEQ     DEPFIL
010600050705     C*
010700050705     C* OPERAZIONI PER NUOVO COMMERCIALE
010800050705     C                   EXSR      NEWAGE
010900050705     C*
011000050705     C* CICLO FINO A ROTTURA DI FILIALE
011100050705DO  5C     FINL00        DOWEQ     '0'                                          --- 4 -->
011200050705     C     WFSDIV        ANDEQ     DEPDIV
011300050705     C     WFSARE        ANDEQ     DEPARE
011400050705     C     WFSFIL        ANDEQ     DEPFIL
011500050705     C     WFSAGE        ANDEQ     DEPAGE
011600991124     C*
011700991124     C* OPERAZIONI PER NUOVO CLIENTE
011800991124     C                   EXSR      NEWCLI
011900991124     C*
012000050705     C* CICLO FINO A ROTTURA DI CLIENTE
012100050705DO  6C     FINL00        DOWEQ     '0'                                          --- 5 -->
012200050705     C     WFSDIV        ANDEQ     DEPDIV
012300050705     C     WFSARE        ANDEQ     DEPARE
012400050705     C     WFSFIL        ANDEQ     DEPFIL
012500050705     C     WFSAGE        ANDEQ     DEPAGE
012600050705     C     WFSKSC        ANDEQ     DEPKSC
012700050705     C*
012800050705     C* MEMORIZZA I DATI DEL CLIENTE
012900050705     C                   EXSR      MEMCLI
013000050705     C*
013100050705     C* SE CLIENTE D TIPO 'AU'=ABILITATO E USA => LEGGO IL FILE CON I DETTAGLI
013200050705     C                   IF        WFSTIP = 'AU'
013300050706     C                   EVAL      STLKSC = WFSKSC
013400050706     C                   EVAL      STLDAT = PARDAI
013500050706     C     KEYSTL        SETLL     SISTL03L
013600050706     C                   IF        %found(SISTL03L)
013700050706     C                   READ      SISTL03L
013800050706     C                   DOW       STLKSC =  WFSKSC AND
013900050706     C                             STLDAT >= PARDAI AND
014000050706     C                             STLDAT <= PARDAF
014100050705     C*
014200050705     C* MEMORIZZA I DATI DEI LOG
014300050705     C                   EXSR      MEMLOG
014400050705     C*
014500050705     C* PROSEGUO CON LA LETTURA DEL FILE DETTAGLI
014600050706     C                   READ      SISTL03L
014700050705     C                   ENDDO
014800050705     C                   ENDIF
014900050705     C                   ENDIF
015000050705     C*
015100050705     C* LEGGO IL CLIENTE SUCCESSIVO
015200050705     C                   EXSR      LETFIL
015300050705     C*
015400050705E   6C                   ENDDO
015500050705     C*
015600050705     C* STAMPA IL TOTALE CLIENTE/DATA O SOLO CLIENTE
015700050705     C                   EXSR      STACLI
015800050705     C*
015900050705E   5C                   ENDDO
016000050705     C*
016100050705     C* STAMPA IL TOTALE COMMERCIALE
016200050705     C                   EXSR      STAAGE
016300991026     C*
016400991026E   4C                   ENDDO                                                  <-- 4 ---
016500000000     C*
016600000000     C* STAMPA IL TOTALE FILIALE
016700000000     C                   EXSR      STAFIL
016800000000     C*
016900000000E   3C                   ENDDO                                                  <-- 3 ---
017000000000     C*
017100000000     C* STAMPA IL TOTALE AREA
017200000000     C                   EXSR      STAARE
017300000000     C*
017400000000E   2C                   ENDDO                                                  <-- 2 ---
017500000000     C*
017600000000     C* STAMPA IL TOTALE DIVISIONE
017700000000     C                   EXSR      STADIV
017800000000     C*
017900000000E   1C                   ENDDO                                                  <-- 1 ---
018000000000     C*
018100000000     C* STAMPA IL TOTALE GENERALE
018200000000     C                   EXSR      STAGEN
018300000000     C*
018400000000     C* STAMPA RIGA FINE STAMPA
018500050705     C                   WRITE     ST39FI
018600000000     C*
018700991123     C                   SETON                                        LR
018800000000     C*------------------------------------------------------------------------*
018900000000     C* LETFIL - LETTURA PROSSIMO RECORD
019000000000     C*------------------------------------------------------------------------*
019100000000     C     LETFIL        BEGSR
019200000000     C*
019300000000     C* LEGGO FINO A:
019400000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
019500000000     C*     - TROVATO RECORD VALIDO
019600000000     C                   MOVEL     'N'           WRECOK
019700000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
019800000000     C     WRECOK        ANDEQ     'N'
019900000000     C*
020000000000     C* LETTURE SUCCESSIVE ALLA PRIMA
020100050705     C                   READ      WFSUV02L                               99
020200000000     C*
020300000000     C     *IN99         IFEQ      '1'                                          *FINE FILE
020400000000     C                   MOVEL     '1'           FINL00
020500000000     C                   ELSE
020600000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
020700050705     C                   ENDIF
020800000000     C*
020900050705     C                   ENDDO                                                  <-- 1 ---
021000000000     C*
021100000000     C                   ENDSR
021200000000     C*------------------------------------------------------------------------*
021300000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
021400000000     C*------------------------------------------------------------------------*
021500050705     C     CHKREC        BEGSR
021600000000     C*
021700000000     C                   MOVEL     'S'           WRECOK            1
021800050706     C*
021900050706     C* VERIFICA DISTRETTO
022000050706     C                   if        WRECOK =  'S'
022100050706     C                   if        PARDIV <> *blanks
022200050706     C                   if        WFSDIV <> PARDIV
022300050706     C                   movel     'N'           WRECOK
022400050706     C                   endif
022500050715     C                   else
022600050715     C     WFSDIV        lookup    skDIG                                  30
022700050715     C                   if        not %found
022800050715     C                   movel     'N'           WRECOK
022900050715     C                   endif
023000050706     C                   endif
023100050706     C                   endif
023200050706     C*
023300050706     C* VERIFICA AREA
023400050706     C                   if        WRECOK =  'S'
023500050706     C                   if        PARARE <> *zeros
023600050706     C                   if        WFSARE <> PARARE
023700050706     C                   movel     'N'           WRECOK
023800050706     C                   endif
023900050715     C                   else
024000050715     C                   movel(p)  WFSARE        wAlfaARE          3
024100050715     C     wAlfaARE      lookup    skARG                                  30
024200050715     C                   if        not %found
024300050715     C                   movel     'N'           WRECOK
024400050715     C                   endif
024500050706     C                   endif
024600050706     C                   endif
024700050706     C*
024800050706     C* VERIFICA P.O.
024900050706     C                   if        WRECOK =  'S'
025000050706     C                   if        PARFIL <> *zeros
025100050706     C                   if        WFSFIL <> PARFIL
025200050706     C                   movel     'N'           WRECOK
025300050706     C                   endif
025400050715     C                   else
025500050715     C                   movel(p)  WFSFIL        wAlfaFIL          3
025600050715     C     wAlfaFIL      lookup    skPOG                                  30
025700050715     C                   if        not %found
025800050715     C                   movel     'N'           WRECOK
025900050715     C                   endif
026000050706     C                   endif
026100050706     C                   endif
026200050706     C*
026300050706     C* VERIFICA COMMERCIALE
026400050706     C                   if        WRECOK =  'S'
026500050706     C                   if        PARAGE <> *zeros
026600050706     C                   if        WFSAGE <> PARAGE
026700050706     C                   movel     'N'           WRECOK
026800050706     C                   endif
026900050706     C                   endif
027000050706     C                   endif
027100050705     C*
027200050705     C* VERIFICA CATEGORIA CLIENTE
027300050705     C                   if        WRECOK =  'S'
027400050705     C                   if        PARCAT <> *blanks
027500050705     C                   if        PARCATFLG =  'I'     AND
027600050705     C                             PARCAT    <> WFSCAT
027700050705     C                   movel     'N'           WRECOK
027800050705     C                   endif
027900050705     C                   if        PARCATFLG = 'E'     AND
028000050705     C                             PARCAT    = WFSCAT
028100050705     C                   movel     'N'           WRECOK
028200050705     C                   endif
028300050705     C                   endif
028400050705     C                   endif
028500050705     C*
028600050705     C* VERIFICA TIPOLOGIA CLIENTE
028700050705     C                   if        WRECOK =  'S'
028800050705     C                   if        PARTIP <> *blanks
028900050705     C                   if        PARTIP <> WFSTIP
029000050705     C                   movel     'N'           WRECOK
029100050705     C                   endif
029200050705     C                   endif
029300050705     C                   endif
029400050705     C*
029500051007     C* VERIFICA SBARRAMENTO SPEDIZIONI (SOLO X "NON ABILITATI")
029600050705     C                   if        WRECOK  =  'S'
029700050705     C                   if        PARSPED <> *zeros
029800051007     C                   if        WFSSPED <  PARSPED AND WFSTIP = 'NA'
029900050705     C                   movel     'N'           WRECOK
030000050705     C                   endif
030100050705     C                   endif
030200050705     C                   endif
030300000000     C*
030400000000     C                   ENDSR
030500000000     C*------------------------------------------------------------------------*
030600050705     C* MEMLOG - MEMORIZZA I DATI DEI LOG
030700000000     C*------------------------------------------------------------------------*
030800050705     C     MEMLOG        BEGSR
030900041217     C*
031000050705     C* TOTALE CLIENTE
031100050706     C                   ADD       STLDRC        TKDRC
031200050706     C                   ADD       STLDFS        TKDFS
031300050706     C                   ADD       STLDTN        TKDTN
031400050706     C                   ADD       STLURC        TKURC
031500050706     C                   ADD       STLUFS        TKUFS
031600050706     C                   ADD       STLUTN        TKUTN
031700050706     C                   ADD       STLTDB        TKTDB
031800050706     C                   ADD       STLORM        TKORM
031900050706     C                   ADD       STLVAG        TKVAG
032000050706     C                   ADD       STLTT2        TKTT2
032100050706     C                   ADD       STLTT3        TKTT3
032200050706     C                   ADD       STLGML        TKGML
032300050706     C                   ADD       STLPOD        TKPOD
032400050705     C* TOTALE COMMERCIALE
032500050706     C                   ADD       STLDRC        TCDRC
032600050706     C                   ADD       STLDFS        TCDFS
032700050706     C                   ADD       STLDTN        TCDTN
032800050706     C                   ADD       STLURC        TCURC
032900050706     C                   ADD       STLUFS        TCUFS
033000050706     C                   ADD       STLUTN        TCUTN
033100050706     C                   ADD       STLTDB        TCTDB
033200050706     C                   ADD       STLORM        TCORM
033300050706     C                   ADD       STLVAG        TCVAG
033400050706     C                   ADD       STLTT2        TCTT2
033500050706     C                   ADD       STLTT3        TCTT3
033600050706     C                   ADD       STLGML        TCGML
033700050706     C                   ADD       STLPOD        TCPOD
033800991027     C* TOTALE FILIALE
033900050706     C                   ADD       STLDRC        TFDRC
034000050706     C                   ADD       STLDFS        TFDFS
034100050706     C                   ADD       STLDTN        TFDTN
034200050706     C                   ADD       STLURC        TFURC
034300050706     C                   ADD       STLUFS        TFUFS
034400050706     C                   ADD       STLUTN        TFUTN
034500050706     C                   ADD       STLTDB        TFTDB
034600050706     C                   ADD       STLORM        TFORM
034700050706     C                   ADD       STLVAG        TFVAG
034800050706     C                   ADD       STLTT2        TFTT2
034900050706     C                   ADD       STLTT3        TFTT3
035000050706     C                   ADD       STLGML        TFGML
035100050706     C                   ADD       STLPOD        TFPOD
035200991027     C* TOTALE AREA
035300050706     C                   ADD       STLDRC        TADRC
035400050706     C                   ADD       STLDFS        TADFS
035500050706     C                   ADD       STLDTN        TADTN
035600050706     C                   ADD       STLURC        TAURC
035700050706     C                   ADD       STLUFS        TAUFS
035800050706     C                   ADD       STLUTN        TAUTN
035900050706     C                   ADD       STLTDB        TATDB
036000050706     C                   ADD       STLORM        TAORM
036100050706     C                   ADD       STLVAG        TAVAG
036200050706     C                   ADD       STLTT2        TATT2
036300050706     C                   ADD       STLTT3        TATT3
036400050706     C                   ADD       STLGML        TAGML
036500050706     C                   ADD       STLPOD        TAPOD
036600991027     C* TOTALE DIVISIONE
036700050706     C                   ADD       STLDRC        TDDRC
036800050706     C                   ADD       STLDFS        TDDFS
036900050706     C                   ADD       STLDTN        TDDTN
037000050706     C                   ADD       STLURC        TDURC
037100050706     C                   ADD       STLUFS        TDUFS
037200050706     C                   ADD       STLUTN        TDUTN
037300050706     C                   ADD       STLTDB        TDTDB
037400050706     C                   ADD       STLORM        TDORM
037500050706     C                   ADD       STLVAG        TDVAG
037600050706     C                   ADD       STLTT2        TDTT2
037700050706     C                   ADD       STLTT3        TDTT3
037800050706     C                   ADD       STLGML        TDGML
037900050706     C                   ADD       STLPOD        TDPOD
038000991027     C* TOTALE GENERALE
038100050706     C                   ADD       STLDRC        TGDRC             7 0
038200050706     C                   ADD       STLDFS        TGDFS             7 0
038300050706     C                   ADD       STLDTN        TGDTN             7 0
038400050706     C                   ADD       STLURC        TGURC             7 0
038500050706     C                   ADD       STLUFS        TGUFS             7 0
038600050706     C                   ADD       STLUTN        TGUTN             7 0
038700050706     C                   ADD       STLTDB        TGTDB             7 0
038800050706     C                   ADD       STLORM        TGORM             7 0
038900050706     C                   ADD       STLVAG        TGVAG             7 0
039000050706     C                   ADD       STLTT2        TGTT2             7 0
039100050706     C                   ADD       STLTT3        TGTT3             7 0
039200050706     C                   ADD       STLGML        TGGML             7 0
039300050706     C                   ADD       STLPOD        TGPOD             7 0
039400991123     C*
039500991123     C                   ENDSR
039600050705     C*------------------------------------------------------------------------*
039700050705     C* MEMCLI - MEMORIZZA I DATI DEL CLIENTE
039800050705     C*------------------------------------------------------------------------*
039900050705     C     MEMCLI        BEGSR
040000050705     C*
040100050705     C* TOTALE CLIENTE
040200050705     C                   ADD       1             TKNCLI
040300050705     C                   ADD       WFSSPED       TKSPED
040400050705     C                   ADD       WFSFATT       TKFATT
040500050705     C                   IF        WFSTIP = 'AU'
040600050705     C                   ADD       1             TKTIPAU
040700050705     C                   ENDIF
040800050705     C                   IF        WFSTIP = 'AN'
040900050705     C                   ADD       1             TKTIPAN
041000050705     C                   ENDIF
041100050705     C                   IF        WFSTIP = 'NA'
041200050705     C                   ADD       1             TKTIPNA
041300050705     C                   ENDIF
041400050705     C* TOTALE COMMERCIALE
041500050705     C                   ADD       1             TCNCLI
041600050705     C                   ADD       WFSSPED       TCSPED
041700050705     C                   ADD       WFSFATT       TCFATT
041800050705     C                   IF        WFSTIP = 'AU'
041900050705     C                   ADD       1             TCTIPAU
042000050705     C                   ENDIF
042100050705     C                   IF        WFSTIP = 'AN'
042200050705     C                   ADD       1             TCTIPAN
042300050705     C                   ENDIF
042400050705     C                   IF        WFSTIP = 'NA'
042500050705     C                   ADD       1             TCTIPNA
042600050705     C                   ENDIF
042700050705     C* TOTALE FILIALE
042800050705     C                   ADD       1             TFNCLI
042900050705     C                   ADD       WFSSPED       TFSPED
043000050705     C                   ADD       WFSFATT       TFFATT
043100050705     C                   IF        WFSTIP = 'AU'
043200050705     C                   ADD       1             TFTIPAU
043300050705     C                   ENDIF
043400050705     C                   IF        WFSTIP = 'AN'
043500050705     C                   ADD       1             TFTIPAN
043600050705     C                   ENDIF
043700050705     C                   IF        WFSTIP = 'NA'
043800050705     C                   ADD       1             TFTIPNA
043900050705     C                   ENDIF
044000050705     C* TOTALE AREA
044100050705     C                   ADD       1             TANCLI
044200050705     C                   ADD       WFSSPED       TASPED
044300050705     C                   ADD       WFSFATT       TAFATT
044400050705     C                   IF        WFSTIP = 'AU'
044500050705     C                   ADD       1             TATIPAU
044600050705     C                   ENDIF
044700050705     C                   IF        WFSTIP = 'AN'
044800050705     C                   ADD       1             TATIPAN
044900050705     C                   ENDIF
045000050705     C                   IF        WFSTIP = 'NA'
045100050705     C                   ADD       1             TATIPNA
045200050705     C                   ENDIF
045300050705     C* TOTALE DIVISIONE
045400050705     C                   ADD       1             TDNCLI
045500050705     C                   ADD       WFSSPED       TDSPED
045600050705     C                   ADD       WFSFATT       TDFATT
045700050705     C                   IF        WFSTIP = 'AU'
045800050705     C                   ADD       1             TDTIPAU
045900050705     C                   ENDIF
046000050705     C                   IF        WFSTIP = 'AN'
046100050705     C                   ADD       1             TDTIPAN
046200050705     C                   ENDIF
046300050705     C                   IF        WFSTIP = 'NA'
046400050705     C                   ADD       1             TDTIPNA
046500050705     C                   ENDIF
046600050705     C* TOTALE GENERALE
046700050705     C                   ADD       1             TGNCLI            6 0
046800050705     C                   ADD       WFSSPED       TGSPED            9 0
046900050705     C                   ADD       WFSFATT       TGFATT           13 3
047000050705     C                   IF        WFSTIP = 'AU'
047100050705     C                   ADD       1             TGTIPAU           6 0
047200050705     C                   ENDIF
047300050705     C                   IF        WFSTIP = 'AN'
047400050705     C                   ADD       1             TGTIPAN           6 0
047500050705     C                   ENDIF
047600050705     C                   IF        WFSTIP = 'NA'
047700050705     C                   ADD       1             TGTIPNA           6 0
047800050705     C                   ENDIF
047900050705     C*
048000050705     C                   ENDSR
048100000000     C*------------------------------------------------------------------------*
048200991124     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
048300000000     C*------------------------------------------------------------------------*
048400000000     C     NEWDIV        BEGSR
048500000000     C*
048600000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
048700050705     C                   MOVEL     WFSDIV        DEPDIV
048800991124     C*
048900991124     C* DECODIFICA LA DIVISIONE
049000991124     C                   EXSR      DECDIV
049100991124     C*
049200991124     C* IMPOSTAZIONI CAMPI IN TESTATA
049300050705     C                   MOVEL(P)  DESDIV        STPDIV
049400000000     C*
049500000000     C* AZZERA I TOTALI DELLA DIVISIONE
049600050705     C                   Z-ADD     *ZEROS        TDNCLI            6 0
049700050705     C                   Z-ADD     *ZEROS        TDSPED            9 0
049800050705     C                   Z-ADD     *ZEROS        TDFATT           13 3
049900050705     C                   Z-ADD     *ZEROS        TDTIPAU           6 0
050000050705     C                   Z-ADD     *ZEROS        TDTIPAN           6 0
050100050705     C                   Z-ADD     *ZEROS        TDTIPNA           6 0
050200991123     C                   Z-ADD     *ZEROS        TDDRC             7 0
050300991123     C                   Z-ADD     *ZEROS        TDDFS             7 0
050400991123     C                   Z-ADD     *ZEROS        TDDTN             7 0
050500991123     C                   Z-ADD     *ZEROS        TDURC             7 0
050600991123     C                   Z-ADD     *ZEROS        TDUFS             7 0
050700991123     C                   Z-ADD     *ZEROS        TDUTN             7 0
050800991123     C                   Z-ADD     *ZEROS        TDTDB             7 0
050900010917     C                   Z-ADD     *ZEROS        TDORM             7 0
051000010917     C                   Z-ADD     *ZEROS        TDVAG             7 0
051100011220     C                   Z-ADD     *ZEROS        TDCDL             7 0
051200011220     C                   Z-ADD     *ZEROS        TDCUL             7 0
051300041217     C                   Z-ADD     *ZEROS        TDTTT             7 0
051400041217     C                   Z-ADD     *ZEROS        TDTTTD            7 0
051500041209     C                   Z-ADD     *ZEROS        TDTT2             7 0
051600041209     C                   Z-ADD     *ZEROS        TDTT3             7 0
051700041209     C                   Z-ADD     *ZEROS        TDGML             7 0
051800041209     C                   Z-ADD     *ZEROS        TDPOD             7 0
051900000000     C*
052000000000     C                   ENDSR
052100000000     C*------------------------------------------------------------------------*
052200000000     C* NEWARE - OPERAZIONI PER NUOVA AREA
052300000000     C*------------------------------------------------------------------------*
052400000000     C     NEWARE        BEGSR
052500000000     C*
052600000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
052700050705     C                   Z-ADD     WFSARE        DEPARE
052800991124     C*
052900991124     C* DECODIFICA L'AREA
053000991124     C                   EXSR      DECARE
053100991124     C*
053200991124     C* IMPOSTAZIONI CAMPI IN TESTATA
053300050705     C                   MOVEL(P)  DESARE        STPARE
053400000000     C*
053500000000     C* AZZERA I TOTALI DELL'AREA
053600050705     C                   Z-ADD     *ZEROS        TANCLI            6 0
053700050705     C                   Z-ADD     *ZEROS        TASPED            9 0
053800050705     C                   Z-ADD     *ZEROS        TAFATT           13 3
053900050705     C                   Z-ADD     *ZEROS        TATIPAU           6 0
054000050705     C                   Z-ADD     *ZEROS        TATIPAN           6 0
054100050705     C                   Z-ADD     *ZEROS        TATIPNA           6 0
054200991123     C                   Z-ADD     *ZEROS        TADRC             7 0
054300991123     C                   Z-ADD     *ZEROS        TADFS             7 0
054400991123     C                   Z-ADD     *ZEROS        TADTN             7 0
054500991123     C                   Z-ADD     *ZEROS        TAURC             7 0
054600991123     C                   Z-ADD     *ZEROS        TAUFS             7 0
054700991123     C                   Z-ADD     *ZEROS        TAUTN             7 0
054800991123     C                   Z-ADD     *ZEROS        TATDB             7 0
054900010917     C                   Z-ADD     *ZEROS        TAORM             7 0
055000010917     C                   Z-ADD     *ZEROS        TAVAG             7 0
055100011220     C                   Z-ADD     *ZEROS        TACDL             7 0
055200011220     C                   Z-ADD     *ZEROS        TACUL             7 0
055300041217     C                   Z-ADD     *ZEROS        TATTT             7 0
055400041217     C                   Z-ADD     *ZEROS        TATTTD            7 0
055500041209     C                   Z-ADD     *ZEROS        TATT2             7 0
055600041209     C                   Z-ADD     *ZEROS        TATT3             7 0
055700041209     C                   Z-ADD     *ZEROS        TAGML             7 0
055800041209     C                   Z-ADD     *ZEROS        TAPOD             7 0
055900000000     C*
056000000000     C                   ENDSR
056100000000     C*------------------------------------------------------------------------*
056200991124     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
056300000000     C*------------------------------------------------------------------------*
056400000000     C     NEWFIL        BEGSR
056500000000     C*
056600000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
056700050705     C                   Z-ADD     WFSFIL        DEPFIL
056800991124     C*
056900991124     C* DECODIFICA LA FILIALE
057000991124     C                   EXSR      DECFIL
057100991124     C*
057200991124     C* IMPOSTAZIONI CAMPI IN TESTATA
057300050705     C                   MOVEL(P)  DESFIL        STPFIL
057400000000     C*
057500000000     C* AZZERA I TOTALI DELLA FILIALE
057600050705     C                   Z-ADD     *ZEROS        TFNCLI            6 0
057700050705     C                   Z-ADD     *ZEROS        TFSPED            9 0
057800050705     C                   Z-ADD     *ZEROS        TFFATT           13 3
057900050705     C                   Z-ADD     *ZEROS        TFTIPAU           6 0
058000050705     C                   Z-ADD     *ZEROS        TFTIPAN           6 0
058100050705     C                   Z-ADD     *ZEROS        TFTIPNA           6 0
058200991123     C                   Z-ADD     *ZEROS        TFDRC             7 0
058300991123     C                   Z-ADD     *ZEROS        TFDFS             7 0
058400991123     C                   Z-ADD     *ZEROS        TFDTN             7 0
058500991123     C                   Z-ADD     *ZEROS        TFURC             7 0
058600991123     C                   Z-ADD     *ZEROS        TFUFS             7 0
058700991123     C                   Z-ADD     *ZEROS        TFUTN             7 0
058800991123     C                   Z-ADD     *ZEROS        TFTDB             7 0
058900010917     C                   Z-ADD     *ZEROS        TFORM             7 0
059000010917     C                   Z-ADD     *ZEROS        TFVAG             7 0
059100011220     C                   Z-ADD     *ZEROS        TFCDL             7 0
059200011220     C                   Z-ADD     *ZEROS        TFCUL             7 0
059300041217     C                   Z-ADD     *ZEROS        TFTTT             7 0
059400041217     C                   Z-ADD     *ZEROS        TFTTTD            7 0
059500041209     C                   Z-ADD     *ZEROS        TFTT2             7 0
059600041209     C                   Z-ADD     *ZEROS        TFTT3             7 0
059700041209     C                   Z-ADD     *ZEROS        TFGML             7 0
059800041209     C                   Z-ADD     *ZEROS        TFPOD             7 0
059900000000     C*
060000000000     C                   ENDSR
060100050705     C*------------------------------------------------------------------------*
060200050705     C* NEWAGE - OPERAZIONI PER NUOVO COMMERCIALE
060300050705     C*------------------------------------------------------------------------*
060400050705     C     NEWAGE        BEGSR
060500050705     C*
060600050705     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
060700050705     C                   Z-ADD     WFSAGE        DEPAGE
060800050705     C                   EVAL      STPAGE = WFSAGED
060900050705     C*
061000050705     C* AZZERA I TOTALI DELLA FILIALE
061100050705     C                   Z-ADD     *ZEROS        TCNCLI            6 0
061200050705     C                   Z-ADD     *ZEROS        TCSPED            9 0
061300050705     C                   Z-ADD     *ZEROS        TCFATT           13 3
061400050705     C                   Z-ADD     *ZEROS        TCTIPAU           6 0
061500050705     C                   Z-ADD     *ZEROS        TCTIPAN           6 0
061600050705     C                   Z-ADD     *ZEROS        TCTIPNA           6 0
061700050705     C                   Z-ADD     *ZEROS        TCDRC             7 0
061800050705     C                   Z-ADD     *ZEROS        TCDFS             7 0
061900050705     C                   Z-ADD     *ZEROS        TCDTN             7 0
062000050705     C                   Z-ADD     *ZEROS        TCURC             7 0
062100050705     C                   Z-ADD     *ZEROS        TCUFS             7 0
062200050705     C                   Z-ADD     *ZEROS        TCUTN             7 0
062300050705     C                   Z-ADD     *ZEROS        TCTDB             7 0
062400050705     C                   Z-ADD     *ZEROS        TCORM             7 0
062500050705     C                   Z-ADD     *ZEROS        TCVAG             7 0
062600050705     C                   Z-ADD     *ZEROS        TCCDL             7 0
062700050705     C                   Z-ADD     *ZEROS        TCCUL             7 0
062800050705     C                   Z-ADD     *ZEROS        TCTTT             7 0
062900050705     C                   Z-ADD     *ZEROS        TCTTTD            7 0
063000050705     C                   Z-ADD     *ZEROS        TCTT2             7 0
063100050705     C                   Z-ADD     *ZEROS        TCTT3             7 0
063200050705     C                   Z-ADD     *ZEROS        TCGML             7 0
063300050705     C                   Z-ADD     *ZEROS        TCPOD             7 0
063400050705     C*
063500050705     C* STAMPA TESTATA SU FOGLIO SUCCESSIVO
063600050705     C     *IN05         IFEQ      *ON                                          *FINE PAGINA
063700050705     C                   WRITE     ST39TE
063800050705     C                   SETOFF                                       05
063900050705     C                   ENDIF
064000050705     C*
064100050705     C                   ENDSR
064200000000     C*------------------------------------------------------------------------*
064300991123     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE/DATA
064400000000     C*------------------------------------------------------------------------*
064500000000     C     NEWCLI        BEGSR
064600000000     C*
064700991124     C* MEMORIZZA NUOVO CLIENTE/DATA IN DEPOSITO
064800050705     C                   MOVEL     WFSRAG        DEPRAG
064900050705     C                   Z-ADD     WFSKSC        DEPKSC
065000050705     C                   MOVEL     WFSCAT        DEPCAT
065100050705     C                   MOVEL     WFSTIP        DEPTIP
065200050705     C                   MOVEL     WFSAGED       DEPAGED
065300000000     C*
065400991124     C* AZZERA I TOTALI DEL CLIENTE/DATA
065500050705     C                   Z-ADD     *ZEROS        TKNCLI            6 0
065600050705     C                   Z-ADD     *ZEROS        TKSPED            9 0
065700050705     C                   Z-ADD     *ZEROS        TKFATT           13 3
065800050705     C                   Z-ADD     *ZEROS        TKTIPAU           6 0
065900050705     C                   Z-ADD     *ZEROS        TKTIPAN           6 0
066000050705     C                   Z-ADD     *ZEROS        TKTIPNA           6 0
066100050705     C                   Z-ADD     *ZEROS        TKDRC             7 0
066200050705     C                   Z-ADD     *ZEROS        TKDFS             7 0
066300050705     C                   Z-ADD     *ZEROS        TKDTN             7 0
066400050705     C                   Z-ADD     *ZEROS        TKURC             7 0
066500050705     C                   Z-ADD     *ZEROS        TKUFS             7 0
066600050705     C                   Z-ADD     *ZEROS        TKUTN             7 0
066700050705     C                   Z-ADD     *ZEROS        TKTDB             7 0
066800050705     C                   Z-ADD     *ZEROS        TKORM             7 0
066900050705     C                   Z-ADD     *ZEROS        TKVAG             7 0
067000050705     C                   Z-ADD     *ZEROS        TKCDL             7 0
067100050705     C                   Z-ADD     *ZEROS        TKCUL             7 0
067200050705     C                   Z-ADD     *ZEROS        TKTTT             7 0
067300050705     C                   Z-ADD     *ZEROS        TKTTTD            7 0
067400050705     C                   Z-ADD     *ZEROS        TKTT2             7 0
067500050705     C                   Z-ADD     *ZEROS        TKTT3             7 0
067600050705     C                   Z-ADD     *ZEROS        TKGML             7 0
067700050705     C                   Z-ADD     *ZEROS        TKPOD             7 0
067800991026     C*
067900000000     C                   ENDSR
068000050705     C*------------------------------------------------------------------------*
068100050705     C* IMPTES - IMPOSTA VALORI D TESTATA
068200050705     C*------------------------------------------------------------------------*
068300050705     C     IMPTES        BEGSR
068400050705     C*
068500050714     C                   IF        PARLAY = 'D'
068600050714     C                   EVAL      STPDES = 'DETTAGLIO'
068700050714     C                   ENDIF
068800050714     C                   IF        PARLAY = 'S'
068900050714     C                   EVAL      STPDES = 'SINTETICA'
069000050714     C                   ENDIF
069100050714     C*
069200050705     C                   MOVEL     PARDAI        WDATA             8
069300050705     C                   EVAL      WDATA = %subst(WDATA:7:2) +
069400050705     C                                     %subst(WDATA:5:2) +
069500050705     C                                     %subst(WDATA:1:4)
069600050705     C                   MOVEL(P)  WDATA         STPDAI
069700050705     C*
069800050705     C                   MOVEL     PARDAF        WDATA
069900050705     C                   EVAL      WDATA = %subst(WDATA:7:2) +
070000050705     C                                     %subst(WDATA:5:2) +
070100050705     C                                     %subst(WDATA:1:4)
070200050705     C                   MOVEL(P)  WDATA         STPDAF
070300050705     C*
070400050705     C                   Z-ADD     PARGIO        STPGG
070500050705     C*
070600050705     C                   MOVEL     WFSDAI        WPERIODO          6
070700050705     C                   EVAL      WPERIODO = %subst(WPERIODO:5:2) +
070800050705     C                                        %subst(WPERIODO:1:4)
070900050705     C                   MOVEL(P)  WPERIODO      STPFATDA
071000050705     C*
071100050705     C                   MOVEL     WFSDAF        WPERIODO
071200050705     C                   EVAL      WPERIODO = %subst(WPERIODO:5:2) +
071300050705     C                                        %subst(WPERIODO:1:4)
071400050705     C                   MOVEL(P)  WPERIODO      STPFATA
071500050706     C*
071600050706     C* Esplicitazione parametri d lancio
071700051010     C                   IF        PARTIP = *blanks
071800051010     C                   EVAL      STPPARAM = 'Clienti abilitati e clienti ' +
071900051010     C                                        'non abilitati con più di '    +
072000051010     C                                        %trim(%editc(PARSPED:'2'))     +
072100050706     C                                        ' spedizioni.'
072200051010     C                   ELSE
072300051010     C                   IF        PARTIP = 'NA'
072400051010     C                   EVAL      STPPARAM = 'Clienti con più di ' +
072500051010     C                                        %trim(%editc(PARSPED:'2'))     +
072600051010     C                                        ' spedizioni.'
072700051010     C                   ENDIF
072800051010     C                   ENDIF
072900050706     C*
073000050706     C                   IF        PARCAT <> *blanks
073100050706     C                   IF        PARCATFLG = 'E'
073200050706     C                   EVAL      STPPARAM = %trim(STPPARAM) + ' ' +
073300050706     C                             'Esclusi clienti categoria ' + PARCAT + '.'
073400050706     C                   ENDIF
073500050706     C                   IF        PARCATFLG = 'I'
073600050706     C                   EVAL      STPPARAM = %trim(STPPARAM) + ' ' +
073700050706     C                             'Solo clienti categoria ' + PARCAT + '.'
073800050706     C                   ENDIF
073900050706     C                   ENDIF
074000050706     C*
074100050706     C                   IF        PARTIP = 'AU'
074200050706     C                   EVAL      STPPARAM = %trim(STPPARAM) + ' ' +
074300050706     C                             'Solo clienti abilitati che usano i servizi.'
074400050706     C                   ENDIF
074500050706     C                   IF        PARTIP = 'AN'
074600050706     C                   EVAL      STPPARAM = %trim(STPPARAM) + ' ' +
074700050706     C                             'Solo clienti abilitati che non usano i ' +
074800050706     C                             'servizi.'
074900050706     C                   ENDIF
075000050706     C                   IF        PARTIP = 'NA'
075100050706     C                   EVAL      STPPARAM = %trim(STPPARAM) + ' ' +
075200050706     C                             'Solo clienti non abilitati.'
075300050706     C                   ENDIF
075400050706     C*
075500050706     C                   IF        PARDIV <> *blanks
075600050706     C                   EVAL      STPPARAM = %trim(STPPARAM) + ' ' +
075700050706     C                             'Solo distretto ' + PARDIV + '.'
075800050706     C                   ENDIF
075900050706     C                   IF        PARARE <> *zeros
076000050706     C                   EVAL      STPPARAM = %trim(STPPARAM) + ' ' +
076100050706     C                             'Solo area '+%trim(%editw(PARARE:'    '))+'.'
076200050706     C                   ENDIF
076300050706     C                   IF        PARFIL <> *zeros
076400050706     C                   EVAL      STPPARAM = %trim(STPPARAM) + ' ' +
076500050706     C                             'Solo P.O. '+%trim(%editw(PARFIL:'    '))+'.'
076600050706     C                   ENDIF
076700050706     C                   IF        PARAGE <> *zeros
076800050706     C                   EVAL      STPPARAM = %trim(STPPARAM) + ' ' +
076900050706     C                             'Solo commerciale ' +
077000050706     C                             %trim(%editw(PARAGE:'        '))+'.'
077100050706     C                   ENDIF
077200050706     C*
077300050705     C                   ENDSR
077400000000     C*------------------------------------------------------------------------*
077500991123     C* STACLI - STAMPA TOTALI CLIENTE/DATA
077600000000     C*------------------------------------------------------------------------*
077700050705     C     STACLI        BEGSR
077800050705     C*
077900050705     C                   CLEAR                   ST39RA
078000050705     C                   CLEAR                   ST39RM
078100991123     C*
078200991123     C                   Z-ADD     DEPKSC        STPKSC
078300991123     C                   MOVEL     DEPRAG        STPRAG
078400050705     C                   EVAL      STPCAT = '   ' + DEPCAT
078500050705     C                   IF        DEPTIP = 'AU'
078600050705     C                   EVAL      STPAU = '   X'
078700050705     C                   ENDIF
078800050705     C                   IF        DEPTIP = 'AN'
078900050705     C                   EVAL      STPAN = '   X'
079000050705     C                   ENDIF
079100050705     C                   IF        DEPTIP = 'NA'
079200050705     C                   EVAL      STPNA = '   X'
079300050705     C                   ENDIF
079400991123     C*
079500050705     C                   Z-ADD     TKSPED        STPSPED
079600050705     C                   Z-ADD     TKFATT        STPFATT
079700050705     C                   Z-ADD     TKTDB         STPTDB
079800050705     C                   Z-ADD     TKORM         STPORM
079900050705     C                   Z-ADD     TKVAG         STPVAG
080000050705     C                   Z-ADD     TKGML         STPGML
080100050705     C                   Z-ADD     TKPOD         STPPOD
080200050705     C                   Z-ADD     TKTT3         STPTT3
080300991123     C*
080400050705     C                   EVAL      STPTDBM=%inth(TKTDB/PARGIO)
080500050705     C                   EVAL      STPORMM=%inth(TKORM/PARGIO)
080600050705     C                   EVAL      STPVAGM=%inth(TKVAG/PARGIO)
080700050705     C                   EVAL      STPGMLM=%inth(TKGML/PARGIO)
080800050705     C                   EVAL      STPPODM=%inth(TKPOD/PARGIO)
080900050705     C                   EVAL      STPTT3M=%inth(TKTT3/PARGIO)
081000991123     C*
081100991123     C* STAMPO RIGA DETTAGLIO
081200991123     C                   EXSR      STRCLI
081300000000     C*
081400000000     C                   ENDSR
081500050705     C*------------------------------------------------------------------------*
081600050705     C* STAAGE - STAMPA TOTALI COMMERCIALE
081700050705     C*------------------------------------------------------------------------*
081800050705     C     STAAGE        BEGSR
081900050705     C*
082000050705     C* IMPOSTO I VALORI DI STAMPA
082100050705     C                   CLEAR                   ST39RA
082200050705     C                   CLEAR                   ST39RM
082300050705     C*
082400050705     C* DESCRIZIONE TOTALE
082500050705     C     TES(5)        CAT       DEPAGED:1     STPRAG
082600050705     C*
082700050705     C                   EVALR     STPCAT  = %EDITC(TCNCLI:'2')
082800050705     C                   EVALR     STPAU   = %EDITC(TCTIPAU:'2')
082900050705     C                   EVALR     STPAN   = %EDITC(TCTIPAN:'2')
083000050705     C                   EVALR     STPNA   = %EDITC(TCTIPNA:'2')
083100050705     C                   Z-ADD     TCSPED        STPSPED
083200050705     C                   Z-ADD     TCFATT        STPFATT
083300050705     C                   Z-ADD     TCTDB         STPTDB
083400050705     C                   Z-ADD     TCORM         STPORM
083500050705     C                   Z-ADD     TCVAG         STPVAG
083600050705     C                   Z-ADD     TCGML         STPGML
083700050705     C                   Z-ADD     TCPOD         STPPOD
083800050705     C                   Z-ADD     TCTT3         STPTT3
083900050705     C*
084000050705     C                   EVAL      STPTDBM=%inth(TCTDB/PARGIO)
084100050705     C                   EVAL      STPORMM=%inth(TCORM/PARGIO)
084200050705     C                   EVAL      STPVAGM=%inth(TCVAG/PARGIO)
084300050705     C                   EVAL      STPGMLM=%inth(TCGML/PARGIO)
084400050705     C                   EVAL      STPPODM=%inth(TCPOD/PARGIO)
084500050705     C                   EVAL      STPTT3M=%inth(TCTT3/PARGIO)
084600050705     C*
084700050705     C* STAMPO RIGA SEPARAZIONE
084800050705     C                   WRITE     ST39RI                                       *RIGA DI SEPARAZIONE
084900050705     C*
085000050705     C* STAMPO RIGA TOTALI
085100050705     C                   EXSR      STRTOT
085200050705     C*
085300050705     C* STAMPO RIGA SEPARAZIONE
085400050705     C                   WRITE     ST39RI                                       *RIGA DI SEPARAZIONE
085500050721     C*
085600050722     C* STAMPO I TOTALI A PAGINA NUOVA SOLO X STAMPA IN DETTAGLIO
085700050721     C                   IF        PARLAY = 'D'
085800050721     C                   SETON                                        05        *SALTO PAG SUCCESSIV
085900050721     C                   ENDIF
086000050705     C*
086100050705     C                   ENDSR
086200000000     C*------------------------------------------------------------------------*
086300000000     C* STAFIL - STAMPA TOTALI FILIALE
086400000000     C*------------------------------------------------------------------------*
086500000000     C     STAFIL        BEGSR
086600050706     C*
086700050722     C* STAMPO I TOTALI A PAGINA NUOVA SOLO X STAMPA IN DETTAGLIO
086800050721     C                   IF        PARLAY = 'D'
086900050706     C                   WRITE     ST39TE
087000050706     C                   SETON                                        05        *SALTO PAG SUCCESSIV
087100050721     C                   ENDIF
087200000000     C*
087300000000     C* IMPOSTO I VALORI DI STAMPA
087400050705     C                   CLEAR                   ST39RA
087500050705     C                   CLEAR                   ST39RM
087600991124     C*
087700991124     C* DESCRIZIONE TOTALE
087800991124     C     TES(1)        CAT       DESFIL:1      STPRAG
087900991123     C*
088000050705     C                   EVALR     STPCAT  = %EDITC(TFNCLI:'2')
088100050705     C                   EVALR     STPAU   = %EDITC(TFTIPAU:'2')
088200050705     C                   EVALR     STPAN   = %EDITC(TFTIPAN:'2')
088300050705     C                   EVALR     STPNA   = %EDITC(TFTIPNA:'2')
088400050705     C                   Z-ADD     TFSPED        STPSPED
088500050705     C                   Z-ADD     TFFATT        STPFATT
088600991123     C                   Z-ADD     TFTDB         STPTDB
088700010917     C                   Z-ADD     TFORM         STPORM
088800010917     C                   Z-ADD     TFVAG         STPVAG
088900041209     C                   Z-ADD     TFGML         STPGML
089000041209     C                   Z-ADD     TFPOD         STPPOD
089100041214     C                   Z-ADD     TFTT3         STPTT3
089200991123     C*
089300050705     C                   EVAL      STPTDBM=%inth(TFTDB/PARGIO)
089400050705     C                   EVAL      STPORMM=%inth(TFORM/PARGIO)
089500050705     C                   EVAL      STPVAGM=%inth(TFVAG/PARGIO)
089600050705     C                   EVAL      STPGMLM=%inth(TFGML/PARGIO)
089700050705     C                   EVAL      STPPODM=%inth(TFPOD/PARGIO)
089800050705     C                   EVAL      STPTT3M=%inth(TFTT3/PARGIO)
089900991026     C*
090000050705     C* STAMPO RIGA SEPARAZIONE
090100050705     C                   WRITE     ST39RI                                       *RIGA DI SEPARAZIONE
090200000000     C*
090300000000     C* STAMPO RIGA TOTALI
090400000000     C                   EXSR      STRTOT
090500050705     C*
090600050705     C* STAMPO RIGA SEPARAZIONE
090700050705     C                   WRITE     ST39RI                                       *RIGA DI SEPARAZIONE
090800000000     C*
090900991123     C                   ENDSR
091000000000     C*------------------------------------------------------------------------*
091100050705     C* STAARE - STAMPA TOTALI AREA
091200000000     C*------------------------------------------------------------------------*
091300000000     C     STAARE        BEGSR
091400050722     C*
091500050722     C* STAMPO I TOTALI A PAGINA NUOVA SOLO X STAMPA IN DETTAGLIO
091600050722     C                   IF        PARLAY = 'D'
091700050722     C                   WRITE     ST39TE
091800050722     C                   SETON                                        05        *SALTO PAG SUCCESSIV
091900050722     C                   ENDIF
092000991124     C*
092100050705     C                   CLEAR                   ST39RA
092200050705     C                   CLEAR                   ST39RM
092300050705     C*
092400991123     C* DESCRIZIONE TOTALE
092500991124     C     TES(2)        CAT       DESARE:1      STPRAG
092600000000     C*
092700991123     C* IMPOSTO I VALORI DI STAMPA
092800050705     C                   EVALR     STPCAT  = %EDITC(TANCLI:'2')
092900050705     C                   EVALR     STPAU   = %EDITC(TATIPAU:'2')
093000050705     C                   EVALR     STPAN   = %EDITC(TATIPAN:'2')
093100050705     C                   EVALR     STPNA   = %EDITC(TATIPNA:'2')
093200050705     C                   Z-ADD     TASPED        STPSPED
093300050705     C                   Z-ADD     TAFATT        STPFATT
093400991123     C                   Z-ADD     TATDB         STPTDB
093500010917     C                   Z-ADD     TAORM         STPORM
093600010917     C                   Z-ADD     TAVAG         STPVAG
093700041209     C                   Z-ADD     TAGML         STPGML
093800041209     C                   Z-ADD     TAPOD         STPPOD
093900041214     C                   Z-ADD     TATT3         STPTT3
094000991123     C*
094100050705     C                   EVAL      STPTDBM=%inth(TATDB/PARGIO)
094200050705     C                   EVAL      STPORMM=%inth(TAORM/PARGIO)
094300050705     C                   EVAL      STPVAGM=%inth(TAVAG/PARGIO)
094400050705     C                   EVAL      STPGMLM=%inth(TAGML/PARGIO)
094500050705     C                   EVAL      STPPODM=%inth(TAPOD/PARGIO)
094600050705     C                   EVAL      STPTT3M=%inth(TATT3/PARGIO)
094700991026     C*
094800000000     C* STAMPO RIGA TOTALE
094900991124     C                   EXSR      STRTOT
095000050705     C                   WRITE     ST39RI                                       *RIGA DI SEPARAZIONE
095100000000     C*
095200000000     C                   ENDSR
095300000000     C*------------------------------------------------------------------------*
095400000000     C* STADIV - STAMPA TOTALI DIVISIONE
095500000000     C*------------------------------------------------------------------------*
095600000000     C     STADIV        BEGSR
095700000000     C*
095800000000     C* STAMPA TOTALE DIVISIONE A PAGINA NUOVA
095900050705     C                   WRITE     ST39TE
096000991123     C                   SETON                                        05        *SALTO PAG SUCCESSIV
096100991123     C*
096200050705     C                   CLEAR                   ST39RA
096300050705     C                   CLEAR                   ST39RM
096400050705     C*
096500991123     C* DESCRIZIONE TOTALE
096600991124     C     TES(3)        CAT       DESDIV:1      STPRAG
096700000000     C*
096800991123     C* IMPOSTO I VALORI DI STAMPA
096900050705     C                   EVALR     STPCAT  = %EDITC(TDNCLI:'2')
097000050705     C                   EVALR     STPAU   = %EDITC(TDTIPAU:'2')
097100050705     C                   EVALR     STPAN   = %EDITC(TDTIPAN:'2')
097200050705     C                   EVALR     STPNA   = %EDITC(TDTIPNA:'2')
097300050705     C                   Z-ADD     TDSPED        STPSPED
097400050705     C                   Z-ADD     TDFATT        STPFATT
097500991123     C                   Z-ADD     TDTDB         STPTDB
097600010917     C                   Z-ADD     TDORM         STPORM
097700010917     C                   Z-ADD     TDVAG         STPVAG
097800041209     C                   Z-ADD     TDGML         STPGML
097900041209     C                   Z-ADD     TDPOD         STPPOD
098000041214     C                   Z-ADD     TDTT3         STPTT3
098100991123     C*
098200050705     C                   EVAL      STPTDBM=%inth(TDTDB/PARGIO)
098300050705     C                   EVAL      STPORMM=%inth(TDORM/PARGIO)
098400050705     C                   EVAL      STPVAGM=%inth(TDVAG/PARGIO)
098500050705     C                   EVAL      STPGMLM=%inth(TDGML/PARGIO)
098600050705     C                   EVAL      STPPODM=%inth(TDPOD/PARGIO)
098700050705     C                   EVAL      STPTT3M=%inth(TDTT3/PARGIO)
098800991026     C*
098900000000     C* STAMPO RIGA TOTALE
099000991123     C                   EXSR      STRTOT
099100050705     C                   WRITE     ST39RI                                       *RIGA DI SEPARAZIONE
099200000000     C*
099300991123     C                   ENDSR
099400000000     C*------------------------------------------------------------------------*
099500000000     C* STAGEN - STAMPA TOTALI GENERALI
099600000000     C*------------------------------------------------------------------------*
099700000000     C     STAGEN        BEGSR
099800000000     C*
099900000000     C* STAMPA TOTALE GENERALE A PAGINA NUOVA
100000050705     C                   MOVEL     *BLANKS       STPAGE
100100991124     C                   MOVEL     *BLANKS       STPFIL
100200991124     C                   MOVEL     *BLANKS       STPARE
100300991124     C                   MOVEL     *BLANKS       STPDIV
100400050705     C                   WRITE     ST39TE
100500991124     C*
100600050705     C                   CLEAR                   ST39RA
100700050705     C                   CLEAR                   ST39RM
100800000000     C*
100900000000     C* DESCRIZIONE TOTALE
101000991123     C                   MOVEL     TES(4)        STPRAG
101100000000     C*
101200991123     C* IMPOSTO I VALORI DI STAMPA
101300050705     C                   EVALR     STPCAT  = %EDITC(TGNCLI:'2')
101400050705     C                   EVALR     STPAU   = %EDITC(TGTIPAU:'2')
101500050705     C                   EVALR     STPAN   = %EDITC(TGTIPAN:'2')
101600050705     C                   EVALR     STPNA   = %EDITC(TGTIPNA:'2')
101700050705     C                   Z-ADD     TGSPED        STPSPED
101800050705     C                   Z-ADD     TGFATT        STPFATT
101900991123     C                   Z-ADD     TGTDB         STPTDB
102000010917     C                   Z-ADD     TGORM         STPORM
102100010917     C                   Z-ADD     TGVAG         STPVAG
102200041209     C                   Z-ADD     TGGML         STPGML
102300041209     C                   Z-ADD     TGPOD         STPPOD
102400041214     C                   Z-ADD     TGTT3         STPTT3
102500991123     C*
102600050705     C                   EVAL      STPTDBM=%inth(TGTDB/PARGIO)
102700050705     C                   EVAL      STPORMM=%inth(TGORM/PARGIO)
102800050705     C                   EVAL      STPVAGM=%inth(TGVAG/PARGIO)
102900050705     C                   EVAL      STPGMLM=%inth(TGGML/PARGIO)
103000050705     C                   EVAL      STPPODM=%inth(TGPOD/PARGIO)
103100050705     C                   EVAL      STPTT3M=%inth(TGTT3/PARGIO)
103200991026     C*
103300000000     C* STAMPO RIGA TOTALE
103400000000     C                   EXSR      STRTOT
103500050705     C                   WRITE     ST39RI                                       *RIGA DI SEPARAZIONE
103600000000     C*
103700000000     C                   ENDSR
103800000000     C*------------------------------------------------------------------------*
103900050705     C* STRCLI - STAMPA RIGA CLIENTE
104000000000     C*------------------------------------------------------------------------*
104100000000     C     STRCLI        BEGSR
104200991123     C*
104300000000     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
104400991123     C     *IN05         IFEQ      '1'
104500000000     C                   SETON                                        01
104600050705     C                   WRITE     ST39RI                                       *RIGA DI SEPARAZIONE
104700050705     C                   WRITE     ST39TE
104800991123     C                   SETOFF                                       05
104900050705     C                   ENDIF
105000000000     C*
105100050705     C* STAMPO RIGA CLIENTE
105200050706     C***  PARDAI        IFNE      PARDAF
105300050706     C                   IF        PARLAY = 'D'
105400050715     C   80              EXSR      WRIWF
105500060504     c                   if        STPsped <> *zeros
105600050714     C   80              WRITE     ST39RA
105700050714     C   81              WRITE     ST39RM
105800060504     c                   endif
105900050706     C                   ENDIF
106000050706     C***                ENDIF
106100000000     C*
106200000000     C                   ENDSR
106300000000     C*------------------------------------------------------------------------*
106400000000     C* STRTOT - STAMPA RIGA TOTALE
106500000000     C*------------------------------------------------------------------------*
106600000000     C     STRTOT        BEGSR
106700991027     C*
106800050705     C* STAMPO RIGA
106900050706     C***  PARDAI        IFNE      PARDAF
107000050715     C                   IF        PARLAY = 'S'
107100050715     C   80              EXSR      WRIWF
107200050715     C                   ENDIF
107300050714     C   80              WRITE     ST39RA
107400050714     C   81              WRITE     ST39RM
107500050714     C***                ENDIF
107600000000     C*
107700000000     C                   ENDSR
107800000000     C*------------------------------------------------------------------------*
107900000000     C* DECDIV - DECODIFICA DIVISIONE
108000000000     C*------------------------------------------------------------------------*
108100000000     C     DECDIV        BEGSR
108200000000     C*
108300050705     C                   MOVEL     1             TBLKUT
108400050705     C                   MOVE      '17'          TBLCOD
108500050705     C                   MOVE      *BLANKS       TBLKEY
108600050705     C                   MOVEL     DEPDIV        TBLKEY
108700000000     C     KEYTAB        CHAIN     TABEL                              97
108800000000     C     *IN97         IFEQ      '1'
108900000000     C     TBLFLG        OREQ      '*'
109000050705     C                   MOVEL     *ALL'*'       DESDIV           20
109100000000     C                   ELSE
109200000000     C                   MOVEL     TBLUNI        DESDIV
109300001115     C                   ENDIF
109400000000     C*
109500000000     C                   ENDSR
109600000000     C*------------------------------------------------------------------------*
109700000000     C* DECARE - DECODIFICA AREA
109800000000     C*------------------------------------------------------------------------*
109900000000     C     DECARE        BEGSR
110000000000     C*
110100050705     C                   MOVEL     1             TBLKUT
110200050705     C                   MOVE      '05'          TBLCOD
110300050705     C                   MOVE      *BLANKS       TBLKEY
110400050705     C                   MOVEL     DEPARE        TBLKEY
110500000000     C     KEYTAB        CHAIN     TABEL                              97
110600000000     C     *IN97         IFEQ      '1'
110700000000     C     TBLFLG        OREQ      '*'
110800050705     C                   MOVEL     *ALL'*'       DESARE           20
110900000000     C                   ELSE
111000000000     C                   MOVEL     TBLUNI        DESARE
111100001115     C                   ENDIF
111200000000     C*
111300000000     C                   ENDSR
111400000000     C*------------------------------------------------------------------------*
111500000000     C* DECFIL - DECODIFICA FILIALE/TERMINAL
111600000000     C*------------------------------------------------------------------------*
111700000000     C     DECFIL        BEGSR
111800000000     C*
111900000000     C                   Z-ADD     1             I
112000000000     C     DEPFIL        LOOKUP    FIL(I)                                 95
112100000000     C     *IN95         IFEQ      '0'
112200050706     C                   MOVEL     *ALL'*'       DESFIL           40
112300000000     C                   ELSE
112400000000     C                   MOVEL     DFIL(I)       DESFIL
112500001115     C                   ENDIF
112600991026     C*
112700991123     C                   ENDSR
112800050715     C*------------------------------------------------------------------------*
112900050715     C* WRIWF - SCRITTURA WRK-FILE
113000050715     C*------------------------------------------------------------------------*
113100050715     C     WRIWF         BEGSR
113200050715     C*
113300050715     C* SOLO SE RICHIESTO NEI PARAMETRI D LANCIO
113400050715     C                   IF        PARFILE = 'S'
113500050715     C*
113600050715     C                   CLEAR                   WFST3900
113700050715     C*
113800050715     C                   EVAL      WFST39DAI  = STPDAI
113900050715     C                   EVAL      WFST39DAF  = STPDAF
114000060504     c                   select
114100060504      * Totale Commerciale
114200060504     c                   when      %subst(STPrag : 1 :
114300060504     c                                %len(%trimr(TES(5))))
114400060504     c                                = %trimr(Tes(5))
114500060504     c                   eval      WFST39trk  = 'C'
114600060504      * Totale P.O.
114700060504     c                   when      %subst(STPrag : 1 :
114800060504     c                                %len(%trimr(TES(1))))
114900060504     c                                = %trimr(Tes(1))
115000060504     c                   eval      WFST39trk  = 'P'
115100060504      * Totale Area
115200060504     c                   when      %subst(STPrag : 1 :
115300060504     c                                %len(%trimr(TES(2))))
115400060504     c                                = %trimr(Tes(2))
115500060504     c                   eval      WFST39trk  = 'A'
115600060504      * Totale Distretto
115700060504     c                   when      %subst(STPrag : 1 :
115800060504     c                                %len(%trimr(TES(3))))
115900060504     c                                = %trimr(Tes(3))
116000060504     c                   eval      WFST39trk  = 'D'
116100060504      * Totale Generale
116200060504     c                   when      %subst(STPrag : 1 :
116300060504     c                                %len(%trimr(TES(4))))
116400060504     c                                = %trimr(Tes(4))
116500060504     c                   eval      WFST39trk  = 'G'
116600060504     c                   endsl
116700050715     C                   EVAL      WFST39DIV  = DEPDIV
116800050715     C                   EVAL      WFST39DIVD = STPDIV
116900050715     C                   EVAL      WFST39ARE  = DEPARE
117000050715     C                   EVAL      WFST39ARED = STPARE
117100050715     C                   EVAL      WFST39FIL  = DEPFIL
117200050715     C                   EVAL      WFST39FILD = STPFIL
117300060504     c                   if            WFST39trk  = *blanks
117400060504     c                             or  WFST39trk  = 'C'
117500050715     C                   EVAL      WFST39AGE  = DEPAGE
117600050715     C                   EVAL      WFST39AGED = STPAGE
117700060504     C                   endif
117800050715     C                   EVAL      WFST39KSC  = STPKSC
117900050715     C                   EVAL      WFST39RAG  = STPRAG
118000050715     C                   EVAL      WFST39CAT  = STPCAT
118100050715     C                   EVAL      WFST39SPE  = STPSPED
118200050715     C                   EVAL      WFST39FATT = STPFATT
118300050715     C                   EVAL      WFST39AU   = %trim(STPAU)
118400050715     C                   EVAL      WFST39AN   = %trim(STPAN)
118500050715     C                   EVAL      WFST39NA   = %trim(STPNA)
118600050715     C                   EVAL      WFST39TDB  = STPTDB
118700050715     C                   EVAL      WFST39ORM  = STPORM
118800050715     C                   EVAL      WFST39TT3  = STPTT3
118900050715     C                   EVAL      WFST39VAG  = STPVAG
119000050715     C                   EVAL      WFST39GML  = STPGML
119100050715     C                   EVAL      WFST39POD  = STPPOD
119200050715     C*
119300050715     C                   WRITE     WFST3900
119400050715     C*
119500050715     C                   ENDIF
119600050715     C*
119700050715     C                   ENDSR
119800050715     C*--------------------------------------------------------------------------------------------*
119900050715     C* REPERISCE I DATI UTENTE
120000050715     C*--------------------------------------------------------------------------------------------*
120100050715     C     REPDATIUTE    BEGSR
120200050715     C*
120300050715     C* INIZIALIZZA VARIABILI DI WRK
120400050715     C                   CLEAR                   TIBS34DS
120500050715     C                   CLEAR                   AZUTEDS
120600050715     C                   CLEAR                   DDATIUTE
120700050715     C*
120800050715     C     *DTAARA       DEFINE    §azute        azuteds
120900050715     C     *DTAARA       DEFINE    §datiute      ddatiute
121000050715     C                   IN(E)     *DTAARA
121100050715     C                   IF        %Error
121200050715     C                   EVAL      I34Tla = 'L'
121300050715     C                   CALL      'TIBS34R'
121400050715     C                   PARM                    Tibs34Ds
121500050715     C                   IN        *DTAARA
121600050715     C                   ENDIF
121700050715     C*
121800050715     C* Verifica errori e autorità profilo
121900050715     C                   IF        DUTERR = 'E'
122000050715     C                   EVAL      FINL00 = '1'
122100050715     C                   ELSE
122200050715     C                   MOVEL(P)  UTEFAF        DUTE01
122300050715     C                   IF        §UTECLI <> *blanks
122400050715     C                   EVAL      wAbi = §UTECLI
122500050715     C                   ELSE
122600050715     C* se non c'è l'abilitazione
122700050715     C* --> se 1° livello, abilitazioni al terminal
122800050715     C*     se 2° livello, abilitazioni al punto operativo
122900050715     C*     se sede è impossibile ma se capita mando a fine il pgm
123000050715     C                   IF        UTEAUT = *blanks
123100050715     C                   IF        DUTLPO = '1'
123200050715     C                   EVAL      wAbi   = 'TP'
123300050715     C                   ENDIF
123400050715     C                   IF        DUTLPO = '2'
123500050715     C                   EVAL      wAbi   = 'PO'
123600050715     C                   ENDIF
123700050715     C                   IF        DUTLPO = 'S'
123800050715     C                   EVAL      wAbi   = 'AZ'
123900050715     C                   ENDIF
124000050715     C                   ELSE
124100050715     C                   EVAL      wAbi = UTEAUT
124200050715     C                   ENDIF
124300050715     C                   ENDIF
124400050715     C                   ENDIF
124500050715     C*
124600050715     C* Reperimento dei P.O. gestibili dall'utente
124700050715     C                   clear                   TRUL31DS
124800060503     C**!!!              clear                   TRUL31DS2
124900050715     C                   eval      I31abi = wAbi
125000050715     C                   eval      I31cdi = DUTdis
125100050715     C                   eval      I31car = DUTare
125200050715     C                   eval      I31cpo = DUTpou
125300050715     C                   call      'TRUL31R'
125400050715     C                   parm                    KPJBA
125500050715     C                   parm                    TRUL31DS
125600060503     C**!!!              parm                    TRUL31DS2
125700050715     C                   if        O31pog > *zeros
125800050715     C                   movea     O31pog        skPOG
125900050715     C                   else
126000050715     C                   movel(p)  DUTpou        wAlfaFIL
126100050715     C                   movea     wAlfaFIL      skPOG
126200050715     C                   endif
126300050715     C*
126400060503     C**!!!              if        O31arg > *zeros
126500060503     C**!!!              movea     O31arg        skARG
126600060503     C**!!!              else
126700060503     C**!!!              movel(p)  DUTare        wAlfaARE
126800060503     C**!!!              movea     wAlfaARE      skARG
126900060503     C**!!!              endif
127000050715     C*
127100060503     C**!!!              if        O31dig > *zeros
127200060503     C**!!!              movea     O31dig        skDIG
127300060503     C**!!!              else
127400060503     C**!!!              movea     DUTdis        skDIG
127500060503     C**!!!              endif
127600060503
127700060503     c                   clear                   skdig
127800060503     c                   clear                   skarg
127900060503      * mi carico la sk dei distretti e aree relativi ai p.o. gestiti dall'utente
128000060503     c                   do        250           xx
128100060503     c                   if        skpog(xx) = *zeros
128200060503     c                   leave
128300060503     c                   endif
128400060503     c                   move      skpog(xx)     w0030
128500060503     c     w0030         chain     azorg01l
128600060503     c                   if        %found(azorg01l)
128700060503      * memorizzo il distretto se non è già stato memorizzato
128800060503     c     orgfl3        lookup    skdig                                  30
128900060503     c                   if        not %found
129000060503     c                   z-add     1             yy
129100060503     c     *blanks       lookup    skdig(yy)                              30
129200060503     c                   if        %found
129300060503     c                   movel     orgfl3        skdig(yy)
129400060503     c                   endif
129500060503     c                   endif
129600060503      * memorizzo l'area se non è già stata memorizzata
129700060503     c                   movel(p)  orgcar        walfaare
129800060503     c     walfaare      lookup    skarg                                  30
129900060503     c                   if        not %found
130000060503     c                   z-add     1             yy
130100060503     c     *blanks       lookup    skarg(yy)                              30
130200060503     c                   if        %found
130300060503     c                   movel     walfaare      skarg(yy)
130400060503     c                   endif
130500060503     c                   endif
130600060503     c                   endif
130700060503     c                   enddo
130800050715     C*
130900050715     C                   ENDSR
131000000000     C*------------------------------------------------------------------------*
131100000000     C* *INZSR - ROUTINE INIZIALE
131200000000     C*------------------------------------------------------------------------*
131300000000     C     *INZSR        BEGSR
131400000000     C*--------------------
131500000000     C* RICEVIMENTO PARAMENTRI
131600000000     C*--------------------
131700000000     C     *ENTRY        PLIST
131800000000     C                   PARM                    KPJBA
131900000000     C                   MOVEL     KPJBU         PARAM
132000050715     C*
132100050715     C* REPERIMENTO DATI UTENTE CORRENTE
132200050715     C                   EXSR      REPDATIUTE
132300000000     C*--------------------
132400000000     C* CAMPI RIFERITI AL DATA BASE
132500000000     C*--------------------
132600050705     C     *LIKE         DEFINE    WFSDIV        DEPDIV
132700050705     C     *LIKE         DEFINE    WFSARE        DEPARE
132800050705     C     *LIKE         DEFINE    WFSFIL        DEPFIL
132900050705     C     *LIKE         DEFINE    WFSAGE        DEPAGE
133000050705     C     *LIKE         DEFINE    WFSRAG        DEPRAG
133100050705     C     *LIKE         DEFINE    WFSKSC        DEPKSC
133200050705     C     *LIKE         DEFINE    WFSTIP        DEPTIP
133300050705     C     *LIKE         DEFINE    WFSCAT        DEPCAT
133400050705     C     *LIKE         DEFINE    WFSAGED       DEPAGED
133500000000     C*
133600000000     C*--------------------
133700000000     C* CHIAVI DI LETTURA
133800000000     C*--------------------
133900050705     C*
134000050705     C* LETTURA TABEL00F - COMPLETA
134100050705     C     KEYTAB        KLIST
134200050705     C                   KFLD                    TBLKUT
134300050705     C                   KFLD                    TBLCOD
134400050705     C                   KFLD                    TBLKEY
134500000000     C*
134600050706     C* CHIAVE LETTURA SISTL03L - COMPLETA
134700050705     C     KEYSTL        KLIST
134800050706     C                   KFLD                    STLKSC
134900050706     C                   KFLD                    STLDAT
135000000000     C*--------------------
135100000000     C* CAMPI DI CONTROLLO
135200000000     C*--------------------
135300000000     C                   MOVEL     '0'           FINL00            1            *FINE FILE
135400991123     C                   SETON                                        05        *OVERFLOW
135500050714     C                   SETON                                        80        *RIGA VALORI ASSOLUT
135600050714     C                   SETOFF                                       81        *RIGA VALORI MEDI
135700050714     C                   SETOFF                                       55        *CAMPI HIDDEN
135800000000     C                   Z-ADD     UDATE         DATA              6 0          *DATA
135900000000     C                   TIME                    ORA               6 0          *ORA
136000000000     C*
136100000000     C* CARICA ORGANIGRAMMA
136200050705     C                   EXSR      CARORG
136300000000     C*
136400000000     C                   ENDSR
136500000000     C*------------------------------------------------------------------------*
136600000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
136700000000     C*------------------------------------------------------------------------*
136800000000     C     CARORG        BEGSR
136900050705     C*
137000050705     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
137100050705     C     *LOVAL        SETLL     AZORG01L                           98
137200050705     C  N98              READ      AZORG01L                               98
137300050705     C                   Z-ADD     0             I                 3 0
137400050705     C     *IN98         DOWEQ     '0'
137500050705     C                   ADD       1             I
137600050705     C                   Z-ADD     ORGFIL        FIL(I)
137700050705     C                   MOVEL     ORGDES        DFIL(I)
137800050705     C                   READ      AZORG01L                               98
137900050705     C                   ENDDO
138000050705     C*
138100000000     C                   ENDSR
138200991123     C*
138300991123     C*------------------------------------------------------------------------*
138400991123     C*
138500000000** -TES-   (DESCRIZIONI  STAMPA)
138600991123TOT P.O.                                                            1
138700991123TOT AREA                                                            2
138800991123TOT DIS.                                                            3
138900991123TOT GENERALE                                                        4
139000050705TOT COMMERCIALE                                                     5
