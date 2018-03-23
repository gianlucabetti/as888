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
003500070207     D* DS PER TISI95R - CONTROLLO DI CAP
003600070207     D tisi95ds      E DS
003700060421     d
003800060421     d kcod            s                   Like(tblCod)
003900060421     d kkey            s                   Like(TblKey)
004000060421     d kdev            s                   Like(cepdev)
004100060421     d p1ddev          s                   Like(cepdev)
004200060421     d PADTOR          s             12
004300060421     d P1DTAF          s             10
004400060421     d P1DCEV          s             20
004500060421     d PaDcev          s             25
004600060421     d P1cfil          s              4
004700941109     C*------------------------------------------------------------------------*
004800930128     C*
004900930128     C* STAMPA INTESTAZIONE STAMPA
005000930128     C                   EXCEPT    TESTA
005100000000     C*
005200060421     C* Elaboro per Data: se impostato anno mi posiziono
005300060421     c                   if        partor='D'
005400060421     c                   if        parann>0
005500060421     c                   z-add     00000101      kdev
005600060421     c                   movel     parann        kdev
005700060421     c     kdev          setll     azcep02l
005800060421     c                   else
005900060421     c     *loval        setll     azcep02l
006000060421     c                   endif
006100060421     c
006200060421     c                   read      azcep02l
006300060421     c                   else
006400060421     c* Ordinamento per cap/p.o.
006500060421     c     *loval        setll     azcep01l
006600060421     c                   read      azcep01l
006700060421     c                   endif
006800060421     c
006900060421     c                   dow       not %eof
007000930128     C*
007100930210     C*
007200930210     C* CONTROLLA SE IL RECORD E' DA STAMPARE
007300930210     C                   EXSR      CHKREC
007400930129     C*
007500930129     C* STAMPA RIGA DI DETTAGLIO TEMPI
007600930210     C     WSALTA        IFEQ      'N'
007700930129     C                   EXSR      STARIG
007800930210     C                   END
007900000000     C*
008000930128     C* LEGGE IL RECORD SUCCESSIVO
008100060421     C     PARTOR        IFEQ      'D'
008200060421     C                   READ      AZCep02L                               98
008300930720     C                   ELSE
008400060421     C                   READ      AZCep01L                               98
008500930720     C                   END
008600930128     C*
008700060421     C                   ENDdo                                                  <-- 1 ---
008800000000     C*
008900930128     C* STAMPA RIGA DI FINE STAMPA
009000000000     C                   EXCEPT    STAFIN
009100000000     C*
009200000000     C                   SETON                                        LR
009300941109     C*------------------------------------------------------------------------*
009400930210     C*? CHKREC - CONTROLLA SE IL RECORD E' DA CONSIDERARE O NO
009500941109     C*------------------------------------------------------------------------*
009600930210     C     CHKREC        BEGSR
009700930210     C*
009800930210     C* IMPOSTA A NO IL FLAG PER LA GESTIONE DEL RECORD
009900930210     C                   MOVEL     'N'           WSALTA            1
010000930210     C*
010100060421     C* Verifio anno, se impostato
010200060421     C                   if        parann>0
010300060421     c                   movel     cepdev        w0040             4 0
010400060421     c                   if        w0040<>parann
010500930210     C                   MOVEL     'S'           WSALTA
010600930210     C                   END
010700060421     C                   END
010800060421     c* Verifico codice evento
010900060421     c                   if        parcev<>*blanks
011000060421     c                             and parcev<>cepcev
011100060421     C                   MOVEL     'S'           WSALTA
011200060421     C                   END
011300941109     C*
011400930210     C                   ENDSR
011500941109     C*------------------------------------------------------------------------*
011600941109     C*? STARIG - STAMPA UNA RIGA
011700941109     C*------------------------------------------------------------------------*
011800930129     C     STARIG        BEGSR
011900070207     c                   setoff                                       10
012000070207     c* chiamo tisi95
012100070207     C*
012200070207     c                   if        cepfil=0
012300070207     C                   CLEAR                   TISI95DS
012400070207     C                   EVAL      I95TCN = '7'
012500070207     C                   EVAL      I95DAT = cepdev
012600070207     C                   EVAL      I95NAR = cepnar
012700070207     C                   EVAL      I95PRV = cepprv
012800070207     C                   EVAL      I95CAP = cepcap
012900070207     C                   EVAL      I95LOC = ceploc
013000070207     C                   CALL      'TISI95R'
013100070207     C                   PARM                    TISI95DS
013200070207     c* Se c'e' la località, memorizzo seobsoleta
013300070207    2c                   if        ceploc<>*blanks  and o95lf1='O'
013400070207     c                   move      'OBS'         cepcap
013500070207     c                   seton                                        10
013600070207     c                   endif
013700070207    2c                   if        ceploc=*blanks  and o95cf1='O'
013800070207     c                   movel     'OBS'         cepcap
013900070207     c                   seton                                        10
014000070207    2c                   endif
014100070207    2c                   endif
014200930129     C*
014300930129     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
014400930129     C     *INOF         IFEQ      '1'
014500930129     C                   EXCEPT    TESTA
014600930129     C                   SETOFF                                       OF
014700930129     C                   END
014800930129     C*
014900930129     C* INVERTO LA DATA DI DECORRENZA
015000060421     C                   Z-ADD     cepdev        G02INV
015100941109     C                   Z-ADD     *ZEROS        G02DAT
015200941109     C                   MOVEL     '3'           G02ERR
015300941109     C                   CALL      'XSRDA8'
015400930129     C                   PARM                    G02PAR
015500060421     C                   Z-ADD     G02DAT        p1ddev
015600930129     C*
015700060421     C* DECODIFICO p.o. se c'è
015800060421     c                   clear                   p1dfil
015900060421     c                   clear                   p1cfil
016000060421     c                   if        cepfil>0
016100060421     c                   movel     cepfil        p1cfil
016200060421     c                   move      '-'           p1cfil
016300060421     C     cepfil        CHAIN     AZORG01L                           99
016400941109     C     *IN99         IFEQ      *OFF
016500060421     C                   MOVEL     ORGDES        p1dfil           20
016600930129     C                   ELSE
016700060421     C                   MOVEL     *BLANKS       p1dfil
016800930129     C                   END
016900060421     C                   END
017000060421     c* Decodifico codice evento
017100060421     c                   clear                   p1dcev
017200060421     c                   eval      kcod='2A'
017300060421     c                   movel(p)  cepcev        kkey
017400060421     c     ktab          chain     tabel00f
017500060421    2c                   if        %found(tabel00f)
017600060421     c                   movel     tbluni        p1dcev
017700060421     c                   endif
017800060421     c* Tipo calcolo affidabilità
017900060421     c                   if        ceptaf='G'
018000060421     c                   eval      p1dtaf='GIUSTIFICA'
018100060421     C                   ELSE
018200060421     c                   eval      p1dtaf='ALLUNGA   '
018300060421     C                   ENDIF
018400930129     C*
018500930129     C* STAMPA RIGA DI DETTAGLIO TEMPI
018600930129     C                   EXCEPT    RIGA
018700930129     C*
018800930129     C                   ENDSR
018900941109     C*------------------------------------------------------------------------*
019000941109     C*?*INZSR -  ROUTINE INIZIALE
019100941109     C*------------------------------------------------------------------------*
019200941109     C     *INZSR        BEGSR
019300000000     C*
019400930129     C* PARAMETRI IN INPUT AL PGM
019500000000     C     *ENTRY        PLIST
019600000000     C                   PARM                    KPJBA
019700970619     C                   MOVEL     KPJBU         PARAM
019800930129     C*
019900060421     c     Ktab          Klist
020000060421     c                   Kfld                    codut             1 0
020100060421     c                   Kfld                    kCod
020200060421     c                   Kfld                    kKey
020300060421     c
020400060421     c                   z-add     1             codut
020500930129     C*
020600930129     C* RECUPERO IL CODICE DELL'AZIENDA
020700930129     C                   Z-ADD     1             CODUT
020800020830     C                   CALL      'X§PARUT'
020900930129     C                   PARM                    UTEDSE
021000930129     C                   MOVEL     RAGUT         RSUT             25
021100000000     C*
021200930129     C* RECUPERO LA DATA E L'ORA
021300060421     c                   time                    w0140            14 0
021400060421     C                   MOVE      w0140         wdata             8 0          *DATA (8) IN G/M/AA
021500930129     C                   TIME                    ORA               6 0
021600060421     c* Dati testata
021700060421     c                   if        partor='D'
021800060421     c                   eval      padtor='Per DATA    '
021900060421     c                   else
022000060421     c                   eval      padtor='Per CAP/P.O.'
022100060421     c                   endif
022200060421     c
022300060421     c* Se impostato, decodifico codice evento
022400060421     c                   eval      kcod='2A'
022500060421     c                   movel(p)  parcev        kkey
022600060421     c     ktab          chain     tabel00f
022700060421    2c                   if        %found(tabel00f)
022800060421     c                   movel     tbluni        padcev
022900060421     c                   endif
023000000000     C*
023100000000     C                   ENDSR
023200941109     O*------------------------------------------------------------------------*
023300941109     O*                 *** STAMPA TEMPI DI TRAZIONE  ***
023400941109     O*------------------------------------------------------------------------*
023500930128     OQSYSPRT   E            TESTA          1 02
023600060421     O                       RSUT                25
023700060424     O                                           65 '** STAMPA CALENDARIO EVENT'
023800060424     O                                         +  0 'I PARTICOLARI **'
023900060424     O                                           95 'TISIP3R'
024000060421     O                                          106 'Data'
024100060421     O                       wdata              117 '  /  /    '
024200060424     O                                          128 'Pagina'
024300930128     O                       PAGE          Z    132
024400941109     O*-------
024500060421     O          E            TESTA       1  1
024600060421     O                                          106 'Ora'
024700060421     O                       ORA                115 '  :  :  '
024800060421     O*-------
024900070207     O          E            TESTA       1  1
025000060421     O                       TES(1)              66
025100060421     O                       padtor              30
025200060421     O                       parann        z     45
025300060421     O                       parcev              68
025400060421     O                       padcev            +  2
025500941109     O*-------
025600070207     O          E            TESTA       1
025700930129     O                       TES(3)              66
025800060421     O                       TES(6)             132
025900941109     O*-------
026000070207     O          E            TESTA       1
026100060421     O                       TES(4)              66
026200060421     O                       TES(7)             132
026300070207     O          E            TESTA       1
026400060421     O                       TES(5)              66
026500070207     O          E            TESTA       1
026600060421     O                       TES(2)              66
026700060421     O                       TES(2)             132
026800941109     O*------------------------
026900070207     O          E            RIGA        1
027000060421     O                       p1ddev                 '  /  /    '
027100060421     O                       cepcev           +   2
027200060421     O                                        +   0 '-'
027300060421     O                       p1dcev           +   0
027400060421     O                       cepdup           +   2
027500060421     O                       p1dtaf           +   2
027600060421     O                       p1cfil           +   5
027700060421     O                       p1dfil           +   0
027800060421     O                       cepnar           +   0
027900060421     O                       cepcap           +   1
028000060421     O                       ceploc           +   1
028100060421     O                       cepprv           +   1
028200070207     O          E    10      RIGA        0  0
028300070207     O                       cepnar              83
028400070207     O                       cepcap           +   1
028500070207     O                       ceploc           +   1
028600070207     O                       cepprv           +   1
028700941109     O*------------------------
028800000000     O          E            STAFIN           60
028900941109     O                                           89 ' F I N E   S T A M P A '
029000000000     O*----------------------------------------------------------------
029100941109**
029200060421Tipo Ordinamento: xxxxxxxxxxxx     Anno: xxxx     Codice Evento:  1
029300060421------------------------------------------------------------------2
029400060421------------   E    V    E    N   T   O   ------------- ----------
029500060421   Data     C o d i c e            Dupli Tipo calcolo   P.Operativ
029600061013                                  cabile Affidabilità
029700060421----------- A  P  P  L  I  C  A  Z  I  O  N  E -------------------6
029800060421o        Nazione /C.A.P. /  Località                           Prv
