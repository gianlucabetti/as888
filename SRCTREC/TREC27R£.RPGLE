000100020806     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* FATTURAZIONE INFRAGRUPPO MCUBI - ESEGUE
000400000000     H* NOTE: Compilare il programma con gli OVRPRTF del clp di guida
000500000000     H*       Non legge lo storico perchè le bolle non ancora stampate rimangono
000600000000     H*       nell'archivio corrente.
000700000000     H*------------------------------------------------------------------------*
000800990408     H* NOTE: Non utilizzare questi indicatori
000900990408     H* 40 41       -> Lettura formati record file
001000100107     H* 01 -> Dettaglio
001100100107     H* 11 -> Riepilogo
001200000000     F*------------------------------------------------------------------------*
001201100107     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300000000     F* DATA BASE
001400000000     F*------------------------------------------------------------------------*
001500000000     F*---
001600000000     F* Mcubi da fatturare
001700000000     F*---
001800990310     FECFIM32C  UF   E           K DISK
001900990419     F*---
002000100107     F* Wfile mc da fatturare x terminal partenza
002100990419     F*---
002200100107     FWFFMD10f  O  A E           K DISK
002300000000     F*---
002400000000     F* Bolle di sede
002500000000     F*---
002600990812     FTITAS30C  IF   E           K DISK
002700990812     F                                     IGNORE(TITAS000)
002800000000     F*---
002900000000     F* Organigramma
003000000000     F*---
003100000000     FAZORG01L  IF   E           K DISK
003200000000     F*---
003300000000     F* Tabelle
003400000000     F*---
003500000000     FTABEL00F  UF A E           K DISK
003600020806     FTNTBE01L  IF   E           K DISK
006701100107     F*---
006702100107     F* RIEPILOGO
006703100107     F*---
006704100107     F* Spool fatture
006705100107     FMCRFATBS  O    E             PRINTER OFLIND(*IN11)
006706100107     F                                     RENAME(EC27TE:EC27TEFB)
006707100107     F                                     RENAME(EC27IN:EC27INFB)
006708100107     F                                     RENAME(EC27DE:EC27DEFB)
006709100107     F                                     RENAME(EC27TO:EC27TOFB)
006710100107     F                                     RENAME(EC27FI:EC27FIFB)
006800000000     F*---
006900990420     F* DETTAGLIO
007000000000     F*---
009200100107     F* Spool fatture
009300000000     FMCDFATBS  O    E             PRINTER OFLIND(*IN01)
009400000000     F                                     RENAME(EC26TE:EC26TEFB)
009500000000     F                                     RENAME(EC26IN:EC26INFB)
009600000000     F                                     RENAME(EC26DE:EC26DEFB)
009700000000     F                                     RENAME(EC26TO:EC26TOFB)
009800000000     F                                     RENAME(EC26FI:EC26FIFB)
009900000000     D*------------------------------------------------------------------------*
010000000000     D* SCHIERE
010100000000     D*------------------------------------------------------------------------*
010200000000     D*---
010300000000     D* DECODIFICA RIGHE IN TESTATA
010400000000     D*---
010500990420     D STM             S             66    DIM(10) CTDATA PERRCD(1)
011100000000     D*---
011200000000     D* ORGANIGRAMMA
011300000000     D*---
011400000000     D ORG             S              3  0 DIM(500)
011500000000     D ODES            S             20    DIM(500)
012100000000     D*---
012200000000     D* DATI DI RIGHE IN STAMPA                          -MULTIPLE X 4 SPOOL-
012300000000     D*---
012400100107     D BDAT            DS                  OCCURS(1) INZ
012500020806     D  LNP                    1     21  0 DIM(7)
012600020806     D  NRS                   22     35  0 DIM(7)
012700020806     D  NSP                   36     84  0 DIM(7)
012800020806     D  MGS                   85    112  0 DIM(7)
012900020806     D  SMC                  113    147  3 DIM(7)
013000000000     D*---
013100000000     D* DS NUMERATORE ALLEGATI FATTURAZIONE INFRAGRUPPO
013200000000     D*---
013300000000     D DS$5          E DS
013400000000     D*---
013500000000     D* DS CONTROLLA DATA (8)
013600000000     D*---
013700000000     D WLBDA8          DS
013800000000     D  G08DAT                 1      8  0
013900000000     D  G08INV                 9     16  0
014000000000     D  G08ERR                17     17
014100000000     D  G08TGI                18     22  0
014200990324     D*---
014300990324     D* DS COMPOSIZIONE DATA -ECFIC-
014400990324     D*---
014500990324     D                 DS
014600990420     D  FIMAAS                 1      4  0
014700990420     D  FIMMES                 5      6  0
014800990420     D  FIMDAT                 1      6  0
014900000000     D*---
015000000000     D* DS COMPOSIZIONE MM/GG GENERICO
015100000000     D*---
015200000000     D                 DS
015300000000     D  $MG1                   1      2  0
015400000000     D  $MG2                   3      4  0
015500000000     D  $MG                    1      4  0
015600010207     D*---
015700010207     D* DS COMPOSIZIONE DATA SPEDIZIONE BOLLA
015800010207     D*---
015900010207     D                 DS
016000010207     D  TASAAS                 1      4  0
016100010207     D  TASMGS                 5      8  0
016200010207     D  TASDSP                 1      8  0
016300000000     D*---
016400990420     D* DS COMPOSIZIONE TOTALE TERMINALs
016500000000     D*---
016600000000     D                 DS
016700000000     D  DSTFPC                 1      3
016800100127     D  DSTFPD                 5     20
017200100127     D  DSTOT                  1     20
017300000000     D*---
017400000000     D* DS PARAMETRI LANCIO
017500000000     D*---
017600000000     D PARAM           DS
017700990324     D  PARANN                 1      4  0
017800990324     D  PARMES                 5      6  0
017900990324     D  PARDAT                 1      6  0
018000990420     D  PARSIM                 7      7
018100000000     D*---
018200000000     D* DS PARAMETRI LANCIO
020401100107     D*---
021400000000     D* ARCHITETTURA
021500000000     D*---
021600000000     D KPJBA         E DS
021700000000     I*------------------------------------------------------------------------*
021800000000     I* INPUT
021900000000     I*------------------------------------------------------------------------*
022000990308     I*---
022100990308     I* INDICATORI LETTURA FORMATI RECORD
022200990308     I*---
022300990408     IECFIM000      40
022400990408     IECFIMP00      41
022500000000     C*------------------------------------------------------------------------*
022600000000     C* MAIN LINE
022700000000     C*------------------------------------------------------------------------*
022800000000     C*
022900000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
023000000000     C                   EXSR      SETFIL
023100000000     C*
023200000000     C* CICLO FINO A FINE FILE
023300000000DO  1C     $FINE         DOWEQ     'N'
023400000000     C*
023500990420     C* OPERAZIONI PER NUOVO TERMINAL PARTENZA / ARRIVO
023600000000     C                   EXSR      NEWTER
023700000000     C*
023800990420     C* CICLO FINO A ROTTURA TERMINAL PARTENZA / ARRIVO
023900000000DO  2C     $FINE         DOWEQ     'N'
024000000000     C     FIMTFP        ANDEQ     DEPTFP
024200000000     C*
024300000000     C* MEMORIZZA E STAMPA DATI BOLLE
024400000000     C                   EXSR      MESDAT
024500000000     C*
024600000000     C* LEGGE IL RECORD SUCCESSIVO
024700000000     C                   EXSR      LETFIL
024800000000E   2C                   ENDDO
024900990420     C*
025000990409     C* STAMPA RIMANENZE BOLLE E TOTALI TFP/TFA -DETTAGLIO E RIEPILOGO-
025100100107     C                   Z-ADD     1             S
025200000000     C     S             OCCUR     BDAT
025300000000     C                   EXSR      STADET
025400000000     C                   EXSR      STATED
025500000000     C                   EXSR      STATER
025501100108     C*
025502100108     c                   exsr      srwtrwf
027400070719      *
027500000000E   1C                   ENDDO
027600000000     C*
027700990420     C* STAMPA IL TOTALE GENERALE E IL FINE ALLEGATO
027800000000     C                   EXSR      STAGED
027900000000     C                   EXSR      STAFID
028000000000     C                   EXSR      STAGER
028100000000     C                   EXSR      STAFIR
028201100108     C* STAMPA LA SINTESI DELL'ELABORAZIONE
028202100108     c                   except    tes
028203100108     c                   except    det
031500000000     C*
031600000000     C                   SETON                                        LR
031700000000     C*------------------------------------------------------------------------*
031800990420     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
031900000000     C*------------------------------------------------------------------------*
032000000000     C     SETFIL        BEGSR
032100000000     C*
032200990310     C     *LOVAL        SETLL     ECFIM32C                           99
032300000000IF  1C     *IN99         IFEQ      *ON
032400000000     C                   MOVEL     'S'           $FINE
032500000000X   1C                   ELSE
032600000000     C                   EXSR      LETFIL
032700000000E   1C                   ENDIF
032800000000     C*
032900000000     C                   ENDSR
033000000000     C*------------------------------------------------------------------------*
033100000000     C* LETFIL - LETTURA PROSSIMO RECORD
033200000000     C*------------------------------------------------------------------------*
033300000000     C     LETFIL        BEGSR
033400990308     C*
033500000000     C* LEGGE FINO A:
033600000000     C*     - FINE FILE
033700000000     C*     - TROVATO RECORD VALIDO
033800000000     C                   MOVEL     'N'           WRECOK
033900000000DO  1C     $FINE         DOWEQ     'N'
034000000000     C     WRECOK        ANDEQ     'N'
034100990309     C*
034200990309     C* SPEGNE GLI INDICATORI DI LETTURA FORMATI RECORD
034300990408     C                   SETOFF                                       40
034400990408     C                   SETOFF                                       41
034500990310     C                   READ      ECFIM32C                               99
034600000000IF  2C     *IN99         IFEQ      *ON
034700000000     C                   MOVEL     'S'           $FINE
034800000000X   2C                   ELSE
034900000000     C                   EXSR      CHKREC
035000000000E   2C                   ENDIF
035100000000E   1C                   ENDDO
035200000000     C*
035300000000     C                   ENDSR
035400000000     C*------------------------------------------------------------------------*
035500000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
035600000000     C*------------------------------------------------------------------------*
035700000000     C     CHKREC        BEGSR
035800000000     C*
035900000000     C                   MOVEL     'S'           WRECOK
036000990310     C*
036100990310     C* BOLLE GIA' STAMPATE, ESCLUDE
036200990310IF  1C     FIMDDO        IFGT      *ZEROS
036300990310     C                   MOVEL     'N'           WRECOK
036400990310E   1C                   ENDIF
036500990324     C*
036600100208     C* PERIODO NON RICHIESTO, ESCLUDE solo mese del lancio
036700100208IF  1C     FIMDAT        IFne      PARDAT
036800990324     C                   MOVEL     'N'           WRECOK
036900990324E   1C                   ENDIF
037000980924     C*
037100990324     C* MC = 0, ESCLUDE
037200980924IF  1C     FIMSMC        IFEQ      *ZEROS
037300980924     C                   MOVEL     'N'           WRECOK
037400980924E   1C                   ENDIF
037500000000     C*
037600000000     C* LEGGE E CONTROLLA LA BOLLA
037700000000IF  1C     WRECOK        IFEQ      'S'
037800000000     C                   EXSR      CHKTAS
037900000000E   1C                   ENDIF
038000000000     C*
038100000000     C* AGGIORNA LA DATA DOCUMENTO DEL RECORD
038200000000IF  1C     WRECOK        IFEQ      'S'
038300000000IF  2C     PARSIM        IFEQ      'N'
038400000000     C                   EXSR      AGGFIM
038500000000E   2C                   ENDIF
038600000000E   1C                   ENDIF
038700000000     C*
038800000000     C                   ENDSR
038900000000     C*------------------------------------------------------------------------*
039000990812     C* CHKTAS - CONTROLLA VALIDITA' RECORD -TITAS-
039100000000     C*------------------------------------------------------------------------*
039200000000     C     CHKTAS        BEGSR
039300000000     C*
039400000000     C* LEGGE LA BOLLA
039500000000     C                   Z-ADD     FIMAAS        KTAAAS
039600000000     C                   Z-ADD     FIMLNP        KTALNP
039700000000     C                   Z-ADD     FIMNRS        KTANRS
039800000000     C                   Z-ADD     FIMNSP        KTANSP
039900000000     C                   MOVEL     FIMTBL        KTATBL
040000990812     C     KEYTAS        CHAIN     TITAS30C                           98
040100000000IF  1C     *IN98         IFEQ      *ON
040200000000     C                   MOVEL     'N'           WRECOK
040300000707E   1C                   ENDIF
042400000000     C*
042500000000     C* MEMORIZZA I DATI DELLA BOLLA
042600000707IF  1C     WRECOK        IFEQ      'S'
042700000000     C                   Z-ADD     TASMGS        $MG
042800000000     C                   MOVEL     $MG2          DEPMGS
042900000000     C                   MOVE      $MG1          DEPMGS
043000980710     C                   MOVEL     TASKSC        N3                3 0
043100990408     C                   Z-ADD     TASLNA        DEPLNA
043200990408     C                   MOVEL     TASCTS        DEPCTS            1
043300990324     C*
046700010207     C                   MOVEL     'B'           DEPSCL
049500990409E   1C                   ENDIF
049600000000     C*
049700000000     C                   ENDSR
049800000000     C*------------------------------------------------------------------------*
049900000000     C* AGGFIM - AGGIORNA LA DATA DOCUMENTO DEL RECORD
050000990308     C*------------------------------------------------------------------------*
050100000000     C     AGGFIM        BEGSR
050200000000     C*
050300990308IF  1C     FIMDDO        IFEQ      *ZEROS
050400000000     C                   Z-ADD     DATCOR        FIMDDO
050500990408IF  2C     *IN40         IFEQ      *ON
050600990308     C                   EXCEPT    FIM000
050700990308E   2C                   ENDIF
050800990408IF  2C     *IN41         IFEQ      *ON
050900990308     C                   EXCEPT    FIMP00
051000990308E   2C                   ENDIF
051100990308E   1C                   ENDIF
051200000000     C*
051300000000     C                   ENDSR
051400000000     C*------------------------------------------------------------------------*
051500990420     C* NEWTER - OPERAZIONI PER NUOVO TERMINAL PARTENZA / ARRIVO
051600000000     C*------------------------------------------------------------------------*
051700000000     C     NEWTER        BEGSR
051802100107     C*
051900000000     C* MEMORIZZA I NUOVI CODICI IN UN DEPOSITO
052000000000     C                   Z-ADD     FIMTFP        DEPTFP
052200000000     C*
052300000000     C* DECODIFICA I CODICI
052400000000     C                   MOVEL     *BLANKS       DESTFP           20
052500000000     C                   Z-ADD     1             I
052600000000     C     DEPTFP        LOOKUP    ORG(I)                                 28
052700000000IF  1C     *IN28         IFEQ      *ON
052800000000     C                   MOVEL     ODES(I)       DESTFP
052900000000E   1C                   ENDIF
053600990409     C*
053700990409     C* AZZERA LE VARIABILI DI LAVORO
053800990611     C                   Z-ADD     *ZEROS        TOTFBS           13 3
054200990409     C*
054300000000     C                   ENDSR
054400000000     C*------------------------------------------------------------------------*
054500000000     C* MESDAT - MEMORIZZA E STAMPA DATI BOLLE
054600000000     C*------------------------------------------------------------------------*
054700000000     C     MESDAT        BEGSR
054800990409     C*---
054900990419     C* ORDINE SPOOL
055000100107     C* . MCDFATBS -> FATTURE
055400000000     C*---
055500000000     C*
055600000000     C* CONTROLLA QUALE SPOOL CONSIDERARE
055901100107     C                   Z-ADD     1             S                 1 0
055902100107     C                   ADD       1             B1                1 0
055903100108     C                   Z-ADD     B1            B                 1 0
056000990419     C*
056100000000     C* MEMORIZZA I DATI DELLA BOLLA
056200990419     C     S             OCCUR     BDAT
056300000000     C                   Z-ADD     FIMLNP        LNP(B)
056400000000     C                   Z-ADD     FIMNRS        NRS(B)
056500000000     C                   Z-ADD     FIMNSP        NSP(B)
056600000000     C                   Z-ADD     DEPMGS        MGS(B)
056700990409     C                   Z-ADD     FIMSMC        SMC(B)
056800000000     C*
056900000000     C* CONTROLLA SE STAMPARE IL BLOCCO DELLE BOLLE MEMORIZZATE
057000000000IF  1C     S             IFEQ      1
057100000000     C                   ADD       FIMSMC        TOTFBS
057200990611     C                   ADD       FIMSMC        TOGFBS           13 3
057300000000IF  2C     B1            IFEQ      7
057400000000     C                   EXSR      STADET
057500000000E   2C                   ENDIF
057600000000E   1C                   ENDIF
057601100107     C                   ENDSR
057602100107     C*------------------------------------------------------------------------*
057603100107     C* srwtrwf - scrive wf per memorizzare i mc per terminal partenza
057604100107     C*------------------------------------------------------------------------*
057605100107     C     srwtrwf       BEGSR
057606100108     c                   if        totfbs <> 0
061511100107     C                   Z-ADD     DEPTFP        FMDTFP
061512100107     C                   Z-ADD     totfbs        FMDSMC
061516100107     C                   WRITE     WFFMD100
061517100108     c                   end
061521100107     C*
061600000000     C                   ENDSR
064500000000     C*------------------------------------------------------------------------*
064600990419     C* STADET - STAMPA IL BLOCCO DELLE BOLLE MEMORIZZATE
064700000000     C*------------------------------------------------------------------------*
064800000000     C     STADET        BEGSR
064900000000     C*
065000990409     C* IMPOSTA I CAMPI DEL FORMATO RECORD
065100000000IF  1C     LNP(1)        IFGT      *ZEROS
065200000000     C                   Z-ADD     LNP(1)        DECLN1
065300000000     C                   Z-ADD     NRS(1)        DECNR1
065400000000     C                   Z-ADD     NSP(1)        DECNS1
065500000000     C                   Z-ADD     MGS(1)        DECMG1
065600000000     C                   Z-ADD     SMC(1)        DECSM1
065700000000     C                   Z-ADD     LNP(2)        DECLN2
065800000000     C                   Z-ADD     NRS(2)        DECNR2
065900000000     C                   Z-ADD     NSP(2)        DECNS2
066000000000     C                   Z-ADD     MGS(2)        DECMG2
066100000000     C                   Z-ADD     SMC(2)        DECSM2
066200000000     C                   Z-ADD     LNP(3)        DECLN3
066300000000     C                   Z-ADD     NRS(3)        DECNR3
066400000000     C                   Z-ADD     NSP(3)        DECNS3
066500000000     C                   Z-ADD     MGS(3)        DECMG3
066600000000     C                   Z-ADD     SMC(3)        DECSM3
066700000000     C                   Z-ADD     LNP(4)        DECLN4
066800000000     C                   Z-ADD     NRS(4)        DECNR4
066900000000     C                   Z-ADD     NSP(4)        DECNS4
067000000000     C                   Z-ADD     MGS(4)        DECMG4
067100000000     C                   Z-ADD     SMC(4)        DECSM4
067200000000     C                   Z-ADD     LNP(5)        DECLN5
067300000000     C                   Z-ADD     NRS(5)        DECNR5
067400000000     C                   Z-ADD     NSP(5)        DECNS5
067500000000     C                   Z-ADD     MGS(5)        DECMG5
067600000000     C                   Z-ADD     SMC(5)        DECSM5
067700000000     C                   Z-ADD     LNP(6)        DECLN6
067800000000     C                   Z-ADD     NRS(6)        DECNR6
067900000000     C                   Z-ADD     NSP(6)        DECNS6
068000000000     C                   Z-ADD     MGS(6)        DECMG6
068100000000     C                   Z-ADD     SMC(6)        DECSM6
068200000000     C                   Z-ADD     LNP(7)        DECLN7
068300000000     C                   Z-ADD     NRS(7)        DECNR7
068400000000     C                   Z-ADD     NSP(7)        DECNS7
068500000000     C                   Z-ADD     MGS(7)        DECMG7
068600000000     C                   Z-ADD     SMC(7)        DECSM7
068700000000     C* FATTURE BAR
068800000000IF  2C     S             IFEQ      1
068900000000     C                   EXSR      STATD1
069000990408     C                   WRITE     EC26DEFB
069100000000     C                   Z-ADD     *ZEROS        B1
069200000000E   2C                   ENDIF
071400000000     C*
071500000000     C* AZZERA LE SCHIERE DI MEMORIZZAZIONE
071600000000     C                   EXSR      AZZSCH
071700000000E   1C                   ENDIF
071800000000     C*
071900000000     C                   ENDSR
072000000000     C*------------------------------------------------------------------------*
072100990419     C* AZZSCH - AZZERA SCHIERE DI MEMORIZZAZIONE
072200000000     C*------------------------------------------------------------------------*
072300000000     C     AZZSCH        BEGSR
072400000000     C*
072500000000     C                   CLEAR                   LNP
072600000000     C                   CLEAR                   NRS
072700000000     C                   CLEAR                   NSP
072800000000     C                   CLEAR                   MGS
072900000000     C                   CLEAR                   SMC
073000000000     C*
073100000000     C                   ENDSR
073200000000     C*------------------------------------------------------------------------*
073300990420     C* STATED - STAMPA TOTALE TERMINAL PARTENZA/ARRIVO
073400000000     C*------------------------------------------------------------------------*
073500000000     C     STATED        BEGSR
073600000000     C*---
073700990419     C* ORDINE SPOOL
073800100107     C* . MCDFATBS -> FATTURE
074200000000     C*---
074300000000     C*
074400990409     C* IMPOSTA I CAMPI DEL FORMATO RECORD
074500000000     C                   MOVEL     *BLANKS       DSTOT
074600000000     C                   MOVEL     DEPTFP        DSTFPC
074700990409     C                   MOVEL     DESTFP        DSTFPD
075100000000     C                   MOVEL     DSTOT         TODTOT
075200990420     C                   MOVEL     STM(8)        TODDES
075300000000     C* FATTURE BAR
075400000000IF  1C     S             IFEQ      1
075500000000IF  2C     TOTFBS        IFNE      *ZEROS
075600990409     C                   EXSR      STATD1
075700000000     C                   Z-ADD     TOTFBS        TOCSMC
075800000000     C                   WRITE     EC26TOFB
075900000000E   2C                   ENDIF
076000000000E   1C                   ENDIF
078800000000     C*
078900000000     C                   ENDSR
079000000000     C*------------------------------------------------------------------------*
079100000000     C* STATER - STAMPA TOTALE TERMINAL PARTENZA/ARRIVO -RIEPILOGO-
079200000000     C*------------------------------------------------------------------------*
079300000000     C     STATER        BEGSR
079400000000     C*---
079500990419     C* ORDINE SPOOL
079600100107     C* . MCRFATBS -> FATTURE
080000000000     C*---
080100000000     C*
080200990409     C* IMPOSTA I CAMPI DEL FORMATO RECORD
080300990420     C                   Z-ADD     DEPTFP        DECTF1
080400990420     C                   MOVEL     DESTFP        DEDTF1
080700000000     C* FATTURE BAR
080800000000IF  1C     S             IFEQ      1
080900000000IF  2C     TOTFBS        IFNE      *ZEROS
081000000000     C                   EXSR      STATR1
081100000000     C                   Z-ADD     TOTFBS        DECSMC
081200000000     C                   WRITE     EC27DEFB
081300000000E   2C                   ENDIF
081400000000E   1C                   ENDIF
084200000000     C*
084300000000     C                   ENDSR
084400990412     C*------------------------------------------------------------------------*
084500990419     C* STAGED - STAMPA TOTALE GENERALE -DETTAGLIO-
084600000000     C*------------------------------------------------------------------------*
084700000000     C     STAGED        BEGSR
084800000000     C*
084900990409     C* IMPOSTA I CAMPI DEL FORMATO RECORD
085000990420     C                   MOVEL     STM(3)        TODTOT
085100990420     C                   MOVEL     *BLANKS       TODDES
085200000000     C*
085300000000     C* FATTURE BAR
085400000000IF  1C     TOGFBS        IFNE      *ZEROS
085500000000     C                   Z-ADD     TOGFBS        TOCSMC
085600000000     C                   EXSR      STATD1
085700000000     C                   WRITE     EC26TOFB
085800000000E   1C                   ENDIF
088000000000     C*
088100000000     C                   ENDSR
088200000000     C*------------------------------------------------------------------------*
088300990419     C* STAGER - STAMPA TOTALE GENERALE -RIEPILOGO-
088400000000     C*------------------------------------------------------------------------*
088500000000     C     STAGER        BEGSR
088600000000     C*
088700990409     C* IMPOSTA I CAMPI DEL FORMATO RECORD
088800990420     C                   MOVEL     STM(3)        TODTOT
088900000000     C*
089000000000     C* FATTURE BAR
089100000000IF  1C     TOGFBS        IFNE      *ZEROS
089200000000     C                   Z-ADD     TOGFBS        TOCSMC
089300000000     C                   EXSR      STATR1
089400000000     C                   WRITE     EC27TOFB
089500000000E   1C                   ENDIF
091700000000     C*
091800000000     C                   ENDSR
091900000000     C*------------------------------------------------------------------------*
092000990419     C* STAFID - STAMPA FINE PROSPETTO -DETTAGLIO-
092100000000     C*------------------------------------------------------------------------*
092200000000     C     STAFID        BEGSR
092300000000     C* FATTURE BAR
092400000000     C                   EXSR      STATD1
092500000000     C                   WRITE     EC26FIFB
093800000000     C*
093900000000     C                   ENDSR
094000000000     C*------------------------------------------------------------------------*
094100990419     C* STAFIR - STAMPA FINE PROSPETTO -RIEPILOGO-
094200000000     C*------------------------------------------------------------------------*
094300000000     C     STAFIR        BEGSR
094400000000     C* FATTURE BAR
094500000000     C                   EXSR      STATR1
094600000000     C                   WRITE     EC27FIFB
095900000000     C*
096000000000     C                   ENDSR
096100000000     C*------------------------------------------------------------------------*
096200100107     C* STATD1 - STAMPA TESTATA DETTAGLIO
096300000000     C*------------------------------------------------------------------------*
096400000000     C     STATD1        BEGSR
096500000000     C*
096600000000IF  1C     *IN01         IFEQ      *ON
096700000000     C                   MOVEL     PARANN        TECALL
096800000000     C                   MOVE      FATBSD        TECALL
096900100121     C                   MOVEL     *blanks       TEDALL
096901100121     C                   MOVEL     STM(4)        TECTES
097000000000     C                   EXSR      DECTEB
097100000000     C                   WRITE     EC26TEFB
097200000000     C                   WRITE     EC26INFB
097300000000     C                   MOVEL     '0'           *IN01
097400000000E   1C                   ENDIF
097500000000     C*
097600000000     C                   ENDSR
103100000000     C*------------------------------------------------------------------------*
103200100107     C* STATR1 - STAMPA TESTATA RIEPILOGO
103300000000     C*------------------------------------------------------------------------*
103400000000     C     STATR1        BEGSR
103500000000     C*
103600000000IF  1C     *IN11         IFEQ      *ON
103700000000     C                   MOVEL     PARANN        TECALL
103800000000     C                   MOVE      FATBSR        TECALL
103900100121     C                   MOVEL     *blanks       TEDALL
104000990420     C                   MOVEL     STM(7)        INDDES
104001100121     C                   MOVEL     STM(4)        TECTES
104100000000     C                   EXSR      DECTEB
104200000000     C                   WRITE     EC27TEFB
104300000000     C                   WRITE     EC27INFB
104400000000     C                   MOVEL     '0'           *IN11
104500000000E   1C                   ENDIF
104600000000     C*
104700000000     C                   ENDSR
110500000000     C*------------------------------------------------------------------------*
110600100107     C* DECTEB - DECODIFICA LA TESTATA
110700000000     C*------------------------------------------------------------------------*
110800000000     C     DECTEB        BEGSR
110900000000     C*
111000100108     C                   MOVEL     *blanks       TECAZN
111001100108     c                   seton                                        88
111002100108     C                   MOVEL     'TREC27R'     TECpgm
111100990324     C                   Z-ADD     PARANN        TECANN
111200990324     C                   Z-ADD     PARMES        TECMES
111400000000     C*
111500000000     C                   ENDSR
112700000000     C*------------------------------------------------------------------------*
112800000000     C* DECPAR - DECODIFICA I PARAMETRI DI LANCIO
112900000000     C*------------------------------------------------------------------------*
113000000000     C     DECPAR        BEGSR
113100000000     C*
113200990324     C                   Z-ADD     PARANN        TECANN
113300990324     C                   Z-ADD     PARMES        TECMES
113400000000IF  1C     PARSIM        IFEQ      'S'
113500000000     C                   MOVEL     STM(10)       TEDSIM
113600000000X   1C                   ELSE
113700000000     C                   MOVEL     *BLANKS       TEDSIM
113800000000E   1C                   ENDIF
114000000000     C*
114100000000     C                   ENDSR
114200000000     C*------------------------------------------------------------------------*
114300000000     C* CARTAB - CARICA LE TABELLE OCCORRENTI
114400000000     C*------------------------------------------------------------------------*
114500000000     C     CARTAB        BEGSR
116000000000     c*---
116100000000     c* ORGANIGRAMMA
116200000000     c*---
116300000000     C                   Z-ADD     *ZEROS        I                 5 0
116400000000     C     *LOVAL        SETLL     AZORG01L
116500000000     C                   READ      AZORG01L                               99
116600000000DO  1C     *IN99         DOWEQ     *OFF
116700000000IF  2C     ORGFVA        IFEQ      *BLANKS
116800000000     C     ORGFAG        ANDNE     'V'
116900000000     C                   ADD       1             I
117000000000     C                   Z-ADD     ORGFIL        ORG(I)
117100000000     C                   MOVEL     ORGDES        ODES(I)
118400000000E   2C                   ENDIF
118500000000     C                   READ      AZORG01L                               99
118600000000E   1C                   ENDDO
118700000000     C*---
118800000000     C* NUMERATORE ALLEGATI FATTURAZIONE INFRAGRUPPO
118900000000     C*---
119000020906     C                   Z-ADD     1             KTBKUT
119100000000     C                   MOVEL     '$5'          KTBCOD
119200000000     C                   MOVEL(P)  PARANN        KTBKEY
119300000000     C     KEYTAB        CHAIN     TABEL00F                           99
119400000000IF  1C     *IN99         IFEQ      *OFF
119500000000     C                   MOVEL     TBLUNI        DS$5
119600000000X   1C                   ELSE
119700000000     C                   CLEAR                   DS$5
119800000000E   1C                   ENDIF
119900990408     C*---
120000000000     C* CALCOLA IL NUMERATORE PER TUTTI GLI ALLEGATI
120100990408     C*---
120200000000     C     §$5NUA        ADD       1             FATBSD
120600100108     C     fatbsd        ADD       1             FATBSR
121000990420     C*
121100100108     C                   Z-ADD     fatbsr        NEWNUA
121200000000     C*
121300000000     C* AGGIORNA IL NUMERATORE
121400000000IF  1C     PARSIM        IFEQ      'N'
121500000000IF  2C     *IN99         IFEQ      *OFF
121600000000     C                   Z-ADD     NEWNUA        §$5NUA
121700000000     C                   MOVEL     DS$5          TBLUNI
121800000000     C                   UPDATE    TABEL
121900000000X   2C                   ELSE
122000020906     C                   Z-ADD     1             TBLKUT
122100000000     C                   MOVEL     '$5'          TBLCOD
122200000000     C                   MOVEL     PARANN        TBLKEY
122300000000     C                   Z-ADD     NEWNUA        §$5NUA
122400000000     C                   MOVEL     DS$5          TBLUNI
122500000000     C                   WRITE     TABEL
122600000000E   2C                   ENDIF
122700000000X   1C                   ELSE
122800000000     C     KEYTAB        SETGT     TABEL00F
122900000000E   1C                   ENDIF
123000000000     C*
123100000000     C                   ENDSR
123200000000     C*------------------------------------------------------------------------*
123300000000     C* *INZSR - OPERAZIONI INIZIALI
123400000000     C*------------------------------------------------------------------------*
123500000000     C     *INZSR        BEGSR
123600000000     C*---
123700000000     C* RICEVIMENTO PARAMETRI
123800000000     C*---
123900000000     C     *ENTRY        PLIST
124000000000     C                   PARM                    KPJBA
124100000000     C                   MOVEL     KPJBU         PARAM
124101100108     C* REPERISCE LA DATA CORRENTE
124102100108     C                   TIME                    N14              14 0
124103100108     C                   MOVE      N14           DATA              8 0
124104100108     C                   MOVEL     N14           ORA               6 0
124500000000     C*---
124600000000     C* VARIABILI RIFERITE AL DATA BASE
124700000000     C*---
124800000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
124900000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
125000000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
125100020806     C     *LIKE         DEFINE    TBECOD        KTECOD
125200990812     C     *LIKE         DEFINE    TASAAS        KTAAAS
125300000000     C     *LIKE         DEFINE    TASLNP        KTALNP
125400000000     C     *LIKE         DEFINE    TASNRS        KTANRS
125500000000     C     *LIKE         DEFINE    TASNSP        KTANSP
125600000000     C     *LIKE         DEFINE    TASTBL        KTATBL
125700990408     C     *LIKE         DEFINE    FIMTFP        DEPTFP
125900000000     C     *LIKE         DEFINE    TASMGS        DEPMGS
126000990408     C     *LIKE         DEFINE    TASSCL        DEPSCL
126100990408     C     *LIKE         DEFINE    TASLNA        DEPLNA
126200000000     C     *LIKE         DEFINE    §$5NUA        FATBSD
126600000000     C     *LIKE         DEFINE    §$5NUA        FATBSR
127000000000     C     *LIKE         DEFINE    §$5NUA        NEWNUA
127100000000     C*---
127200000000     C* CHIAVI DI LETTURA
127300000000     C*---
127400000000     C     KEYTAB        KLIST
127500000000     C                   KFLD                    KTBKUT
127600000000     C                   KFLD                    KTBCOD
127700000000     C                   KFLD                    KTBKEY
127800990812     C     KEYTAS        KLIST
127900000000     C                   KFLD                    KTAAAS
128000000000     C                   KFLD                    KTALNP
128100000000     C                   KFLD                    KTANRS
128200000000     C                   KFLD                    KTANSP
128300000000     C                   KFLD                    KTATBL
128400020806     C     KEYTB2        KLIST
128500020806     C                   KFLD                    KTECOD
128600000000     C*--------------------
128700000000     C* VARIABILI DI CONTROLLO
128800000000     C*--------------------
128900000000     C                   MOVEL     'N'           $FINE             1
129000000000     C                   MOVEL     'N'           WRECOK            1
129200000000     C                   MOVEL     '1'           *IN01
129300000000     C                   MOVEL     '1'           *IN11
130000000000     C*
130100000000     C* REPERISCE LA DATA CORRENTE
130200000000     C                   TIME                    N14              14 0
130300000000     C                   MOVE      N14           N8                8 0
130400000000     C                   Z-ADD     N8            G08DAT
130500000000     C                   Z-ADD     *ZEROS        G08INV
130600000000     C                   MOVEL     '0'           G08ERR
130700000000     C                   CALL      'XSRDA8'
130800000000     C                   PARM                    WLBDA8
130900000000     C                   Z-ADD     G08INV        DATCOR            8 0
131000000000     C*
131100000000     C* CARICA LE TABELLE OCCORRENTI
131200000000     C                   EXSR      CARTAB
131300000000     C*
131400000000     C* DECODIFICA I PARAMETRI DI LANCIO
131500000000     C                   EXSR      DECPAR
131600000000     C*
131700000000     C                   ENDSR
134300000000     O*------------------------------------------------------------------------*
134400000000     O* OUTPUT
134500000000     O*------------------------------------------------------------------------*
134600990308     OECFIM000  E            FIM000
134700000000     O                       FIMDDO
134800990308     OECFIMP00  E            FIMP00
134900990308     O                       FIMDDO
134901100107     OQSYSPRT   E            TES            2 02
134902100107     O                       STM(1)              66
134903100107     O                       STM(2)             132
134905100107     O                       PARANN        Z     80
134906100107     O                       PARMES        Z     83
134907100107     O                       TEDSIM              99
134908100107     O                       DATA          8    111
134909100107     O                       ORA                121 '0 :  :  '
134910100107     O                       PAGE          Z    132
134911100107     O          E            DET         1
134912100107     O                       STM(5)              66
134913100127     O                       parann        Z     39
134914100127     O                       FATBSD              42
134915100108     O                       TOGFBS        O     75
134928100107     O          E            DET         1
134929100107     O                       STM(6)              66
134930100127     O                       parann        Z     39
134931100127     O                       FATBSR              42
134932100108     O                       TOGFBS        O     75
135000000000     O*------------------------------------------------------------------------*
135100990420**   STM - DECODIFICHE RIGHE DI STAMPA
135101100108TREC27R                       SINTESI FATTURAZIONE             MC 1
135102100107ANNO MESE 9999/99     XXXXXXXXXXX  99/99/9999  99:99:99   PAG.99992
135400990420GENERALE                                                          3
135500100121QUANTITA' FATTURATE PER TRAZIONI                                  4
135601100108FATTURE                     ALL.N°          DETTAGLIO             5
135701100108FATTURE                     ALL.N°          RIEPILOGO             6
135800100107TERMINAL PARTENZA                                                 7
135900100107TERM. PARTENZA                                                    8
1360001001079
136100990420SIMULAZIONE                                                       10
