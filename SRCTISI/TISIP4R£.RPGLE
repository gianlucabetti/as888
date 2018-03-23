000100941109     H*------------------------------------------------------------------------*
000200060421     H* STAMPA Eventi Particolari - ESEGUE                                     *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500941109     F*------------------------------------------------------------------------*
000600941109     F* DATA BASE
000700941109     F*------------------------------------------------------------------------*
000800060421     FAZcep01L  IF   E           K DISK
000900060421     FAZcep02L  IF   E           K DISK    rename(azcep000:azcep002)
001000930129     FAZORG01L  IF   E           K DISK
001100941109     FTABEL00F  IF   E           K DISK
001200000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300000000     F                                     FORMLEN(66)
001400000000     F                                     FORMOFL(60)
001500941109     D*------------------------------------------------------------------------*
001600060421     D TES             S             66    DIM(7) CTDATA PERRCD(1)
001700941109     D*-------------------
001800930210     D* DS PARAMETRI DI LANCIO PGM
001900941109     D*-------------------
002000970619     D PARAM           DS
002100060421     D  PARTOR                 1      1
002200060421     D  PARann                 2      5  0
002300060421     D  PARCEV                 6      8
002400941109     D*-------------------
002500941109     D* DS CONTROLLO DATE
002600941109     D*-------------------
002700930129     D G02PAR          DS
002800941109     D  G02DAT                 1      8  0
002900941109     D  G02INV                 9     16  0
003000941109     D  G02ERR                17     17
003100941109     D  G02TGI                18     22  0
003200941109     D*-------------------
003300000000     D KPJBA         E DS
003400020906     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003500060421     d
003600060421     d kcod            s                   Like(tblCod)
003700060421     d kkey            s                   Like(TblKey)
003800060421     d kdev            s                   Like(cepdev)
003900060421     d p1ddev          s                   Like(cepdev)
004000060421     d PADTOR          s             12
004100060421     d P1DTAF          s             10
004200060421     d P1DCEV          s             20
004300060421     d PaDcev          s             25
004400060421     d P1cfil          s              4
004500941109     C*------------------------------------------------------------------------*
004600930128     C*
004700930128     C* STAMPA INTESTAZIONE STAMPA
004800930128     C                   EXCEPT    TESTA
004900000000     C*
005000060421     C* Elaboro per Data: se impostato anno mi posiziono
005100060421     c                   if        partor='D'
005200060421     c                   if        parann>0
005300060421     c                   z-add     00000101      kdev
005400060421     c                   movel     parann        kdev
005500060421     c     kdev          setll     azcep02l
005600060421     c                   else
005700060421     c     *loval        setll     azcep02l
005800060421     c                   endif
005900060421     c
006000060421     c                   read      azcep02l
006100060421     c                   else
006200060421     c* Ordinamento per cap/p.o.
006300060421     c     *loval        setll     azcep01l
006400060421     c                   read      azcep01l
006500060421     c                   endif
006600060421     c
006700060421     c                   dow       not %eof
006800930128     C*
006900930210     C*
007000930210     C* CONTROLLA SE IL RECORD E' DA STAMPARE
007100930210     C                   EXSR      CHKREC
007200930129     C*
007300930129     C* STAMPA RIGA DI DETTAGLIO TEMPI
007400930210     C     WSALTA        IFEQ      'N'
007500930129     C                   EXSR      STARIG
007600930210     C                   END
007700000000     C*
007800930128     C* LEGGE IL RECORD SUCCESSIVO
007900060421     C     PARTOR        IFEQ      'D'
008000060421     C                   READ      AZCep02L                               98
008100930720     C                   ELSE
008200060421     C                   READ      AZCep01L                               98
008300930720     C                   END
008400930128     C*
008500060421     C                   ENDdo                                                  <-- 1 ---
008600000000     C*
008700930128     C* STAMPA RIGA DI FINE STAMPA
008800000000     C                   EXCEPT    STAFIN
008900000000     C*
009000000000     C                   SETON                                        LR
009100941109     C*------------------------------------------------------------------------*
009200930210     C*? CHKREC - CONTROLLA SE IL RECORD E' DA CONSIDERARE O NO
009300941109     C*------------------------------------------------------------------------*
009400930210     C     CHKREC        BEGSR
009500930210     C*
009600930210     C* IMPOSTA A NO IL FLAG PER LA GESTIONE DEL RECORD
009700930210     C                   MOVEL     'N'           WSALTA            1
009800930210     C*
009900060421     C* Verifio anno, se impostato
010000060421     C                   if        parann>0
010100060421     c                   movel     cepdev        w0040             4 0
010200060421     c                   if        w0040<>parann
010300930210     C                   MOVEL     'S'           WSALTA
010400930210     C                   END
010500060421     C                   END
010600060421     c* Verifico codice evento
010700060421     c                   if        parcev<>*blanks
010800060421     c                             and parcev<>cepcev
010900060421     C                   MOVEL     'S'           WSALTA
011000060421     C                   END
011100941109     C*
011200930210     C                   ENDSR
011300941109     C*------------------------------------------------------------------------*
011400941109     C*? STARIG - STAMPA UNA RIGA
011500941109     C*------------------------------------------------------------------------*
011600930129     C     STARIG        BEGSR
011700930129     C*
011800930129     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
011900930129     C     *INOF         IFEQ      '1'
012000930129     C                   EXCEPT    TESTA
012100930129     C                   SETOFF                                       OF
012200930129     C                   END
012300930129     C*
012400930129     C* INVERTO LA DATA DI DECORRENZA
012500060421     C                   Z-ADD     cepdev        G02INV
012600941109     C                   Z-ADD     *ZEROS        G02DAT
012700941109     C                   MOVEL     '3'           G02ERR
012800941109     C                   CALL      'XSRDA8'
012900930129     C                   PARM                    G02PAR
013000060421     C                   Z-ADD     G02DAT        p1ddev
013100930129     C*
013200060421     C* DECODIFICO p.o. se c'è
013300060421     c                   clear                   p1dfil
013400060421     c                   clear                   p1cfil
013500060421     c                   if        cepfil>0
013600060421     c                   movel     cepfil        p1cfil
013700060421     c                   move      '-'           p1cfil
013800060421     C     cepfil        CHAIN     AZORG01L                           99
013900941109     C     *IN99         IFEQ      *OFF
014000060421     C                   MOVEL     ORGDES        p1dfil           20
014100930129     C                   ELSE
014200060421     C                   MOVEL     *BLANKS       p1dfil
014300930129     C                   END
014400060421     C                   END
014500060421     c* Decodifico codice evento
014600060421     c                   clear                   p1dcev
014700060421     c                   eval      kcod='2A'
014800060421     c                   movel(p)  cepcev        kkey
014900060421     c     ktab          chain     tabel00f
015000060421    2c                   if        %found(tabel00f)
015100060421     c                   movel     tbluni        p1dcev
015200060421     c                   endif
015300060421     c* Tipo calcolo affidabilità
015400060421     c                   if        ceptaf='G'
015500060421     c                   eval      p1dtaf='GIUSTIFICA'
015600060421     C                   ELSE
015700060421     c                   eval      p1dtaf='ALLUNGA   '
015800060421     C                   ENDIF
015900930129     C*
016000930129     C* STAMPA RIGA DI DETTAGLIO TEMPI
016100930129     C                   EXCEPT    RIGA
016200930129     C*
016300930129     C                   ENDSR
016400941109     C*------------------------------------------------------------------------*
016500941109     C*?*INZSR -  ROUTINE INIZIALE
016600941109     C*------------------------------------------------------------------------*
016700941109     C     *INZSR        BEGSR
016800000000     C*
016900930129     C* PARAMETRI IN INPUT AL PGM
017000000000     C     *ENTRY        PLIST
017100000000     C                   PARM                    KPJBA
017200970619     C                   MOVEL     KPJBU         PARAM
017300930129     C*
017400060421     c     Ktab          Klist
017500060421     c                   Kfld                    codut             1 0
017600060421     c                   Kfld                    kCod
017700060421     c                   Kfld                    kKey
017800060421     c
017900060421     c                   z-add     1             codut
018000930129     C*
018100930129     C* RECUPERO IL CODICE DELL'AZIENDA
018200930129     C                   Z-ADD     1             CODUT
018300020830     C                   CALL      'X§PARUT'
018400930129     C                   PARM                    UTEDSE
018500930129     C                   MOVEL     RAGUT         RSUT             25
018600000000     C*
018700930129     C* RECUPERO LA DATA E L'ORA
018800060421     c                   time                    w0140            14 0
018900060421     C                   MOVE      w0140         wdata             8 0          *DATA (8) IN G/M/AA
019000930129     C                   TIME                    ORA               6 0
019100060421     c* Dati testata
019200060421     c                   if        partor='D'
019300060421     c                   eval      padtor='Per DATA    '
019400060421     c                   else
019500060421     c                   eval      padtor='Per CAP/P.O.'
019600060421     c                   endif
019700060421     c
019800060421     c* Se impostato, decodifico codice evento
019900060421     c                   eval      kcod='2A'
020000060421     c                   movel(p)  parcev        kkey
020100060421     c     ktab          chain     tabel00f
020200060421    2c                   if        %found(tabel00f)
020300060421     c                   movel     tbluni        padcev
020400060421     c                   endif
020500000000     C*
020600000000     C                   ENDSR
020700941109     O*------------------------------------------------------------------------*
020800941109     O*                 *** STAMPA TEMPI DI TRAZIONE  ***
020900941109     O*------------------------------------------------------------------------*
021000930128     OQSYSPRT   E            TESTA          1 02
021100060421     O                       RSUT                25
021200060424     O                                           65 '** STAMPA CALENDARIO EVENT'
021300060424     O                                         +  0 'I PARTICOLARI **'
021400060424     O                                           95 'TISIP3R'
021500060421     O                                          106 'Data'
021600060421     O                       wdata              117 '  /  /    '
021700060424     O                                          128 'Pagina'
021800930128     O                       PAGE          Z    132
021900941109     O*-------
022000060421     O          E            TESTA       1  1
022100060421     O                                          106 'Ora'
022200060421     O                       ORA                115 '  :  :  '
022300060421     O*-------
022400060421     O          E            TESTA       1  2
022500060421     O                       TES(1)              66
022600060421     O                       padtor              30
022700060421     O                       parann        z     45
022800060421     O                       parcev              68
022900060421     O                       padcev            +  2
023000941109     O*-------
023100930129     O          E            TESTA          1
023200930129     O                       TES(3)              66
023300060421     O                       TES(6)             132
023400941109     O*-------
023500930129     O          E            TESTA          1
023600060421     O                       TES(4)              66
023700060421     O                       TES(7)             132
023800060421     O          E            TESTA          1
023900060421     O                       TES(5)              66
024000060421     O          E            TESTA          1
024100060421     O                       TES(2)              66
024200060421     O                       TES(2)             132
024300941109     O*------------------------
024400930128     O          E            RIGA           1
024500060421     O                       p1ddev                 '  /  /    '
024600060421     O                       cepcev           +   2
024700060421     O                                        +   0 '-'
024800060421     O                       p1dcev           +   0
024900060421     O                       cepdup           +   2
025000060421     O                       p1dtaf           +   2
025100060421     O                       p1cfil           +   5
025200060421     O                       p1dfil           +   0
025300060421     O                       cepnar           +   0
025400060421     O                       cepcap           +   1
025500060421     O                       ceploc           +   1
025600060421     O                       cepprv           +   1
025700941109     O*------------------------
025800000000     O          E            STAFIN           60
025900941109     O                                           89 ' F I N E   S T A M P A '
026000000000     O*----------------------------------------------------------------
026100941109**
026200060421Tipo Ordinamento: xxxxxxxxxxxx     Anno: xxxx     Codice Evento:  1
026300060421------------------------------------------------------------------2
026400060421------------   E    V    E    N   T   O   ------------- ----------
026500060421   Data     C o d i c e            Dupli Tipo calcolo   P.Operativ
026600060421                                  cabile Responsabilità
026700060421----------- A  P  P  L  I  C  A  Z  I  O  N  E -------------------6
026800060421o        Nazione /C.A.P. /  Località                           Prv
