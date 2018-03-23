000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000101011128     F*----------------------------------------------------*
000400000000     F*          DESCRIZIONI AGGIUNTIVE MANUTENZIONI       *
000800000000     F*----------------------------------------------------*
000900011210     FmzNOT01L  UF A E           K DISK
000901011129     Fmzame01l  iF   E           K DISK
001000011128     FTRMZ26D3  CF   E             WORKSTN
001100000000     F                                     SFILE(GA26D3SF:NRR)
001200000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
001300000000     D KPJBA         E DS
001301011128     D PARAM           DS
001302011128     D* numero nota
001303011129     D  ParNRF                        7  0
001304011129     D  Partip                        2
001305011129     D  ParNOT                        7  0
001306011128     c* modalità con cui chiamo il pgm
001307011129     D  parmod                        1
001308011128     c* Cmd di ritorno
001309011129     D  parret                        1
001310011129     c*
005702011129     C                   if        not *inlr
006300000000     C                   EXSR      PULIZ
006400000000     C* CARICAMENTO SUBFILE
006500000000     C                   EXSR      CARIC
007200891106     C                   WRITE     GA26D301
007201011129     c*
007300011128     C                   DO        *HIVAL
007301011221     c                   if        *in20
007302011221     c                   write     ga26d3sc
007303011221     c                   EXFMT     PROTECT
007304011221     C                   ELSE
007400000000     C                   EXFMT     GA26D3SC
007401011221     c                   end
007402011129     c* fine
007403011129     c                   if        *inkc
007405011129     c                   movel     'C'           parret
007406011129     c                   leave
007407011129     C                   END
007408011129     c* rollup
007500000000     C                   SETOFF                                           71
007600000000     C   60              EXSR      ROLLUP
007700000000     C   60              SETON                                            71
007701011129     c* conferma
007702011129     c                   if        *inkf
008800000000     C                   EXSR      ANNUL
008900000000     C                   EXSR      SCRIVI
008901011129     c                   movel     'F'           parret
008902011129     c                   leave
009000000000     C                   END
009001011129     C                   ENDdo
009002011129     C                   END
009003011129     c                   seton                                        lr
009400000000     C                   MOVEL     PARAM         KPJBU
009500000000     C******************************************************
009800000000     C** PULIZIA DEL SUBFILE
009801011129     C******************************************************
009901011129     C     PULIZ         BEGSR
010000000000     C                   SETON                                        70
010100000000     C                   SETOFF                                       71
010200891106     C                   WRITE     GA26D3SC
010300000000     C                   SETON                                        71
010400000000     C                   SETOFF                                       70
010500000000     C                   ENDSR
010600000000     C******************************************************
010601011129     C** CARICAMENTO SUBFILE
010602011129     C******************************************************
010700000000     C     CARIC         BEGSR
011300000000     C                   Z-ADD     0             NRR               4 0
011400011210     C     KNOT          SETLL     mzNOT000
011401011129     c                   do        *hival
011500011210     C     KNOT          READE     mzNOT000                               01
011600011129     C   01              leave
011700000000     C                   MOVEL     NOTDES        S1DES
011800000000     C                   ADD       1             NRR
011900000000     C                   WRITE     GA26D3SF
012100000000     C                   END
012200000000     C                   Z-ADD     NRR           SAVNRR
012300000000     C* CARICO RECORDS VUOTI
012400000000     C                   EXSR      ROLLUP
012500000000     C                   ENDSR
012600000000     C******************************************************
012900000000     C** ANNULLAMENTO DEI RECORD
012901011129     C******************************************************
013101011129     C     ANNUL         BEGSR
013200000000     C                   DO        *HIVAL
013300011210     C     KNOT          DELETE    mzNOT000                           21
013400000000     C  N21              END
013500000000     C                   ENDSR
013501011129     C******************************************************
013900000000     C** SCRITTURA DEI NUOVI
014000000000     C** RECORD
014001011129     C******************************************************
014101011129     C     SCRIVI        BEGSR
014200000000     C* VEDO SE HA AGGIUNTO   ALMENO UNA DESCRIZIONE
014300000000     C* SE SI' CREO NUOVO NUMERO
014400000000     C     PARNOT        IFEQ      0
014500891106     C                   READC     GA26D3SF                               21
014600000000     C     *IN21         DOWEQ     '0'
014700000000     C     S1DES         COMP      *BLANK                                 22    22
014800000000     C                   READC     GA26D3SF                               21
014900011129     C  N22              ENDdo
015100000000     C  N22              DO
015200011210     C     *LOCK         IN        mzNOT
015300011210     C                   ADD       1             mzNOT
015400011210     C                   OUT       mzNOT
015500011210     C                   Z-ADD     mzNOT         PARNOT
015600011129     C                   ENDdo
015700011129     C                   ENDif
015701011129     C*
015800000000     C                   Z-ADD     PARNOT        NOTNOT
015801011129     c                   z-add     0             parnot
016000000000      *
016100000000     C                   DO        *HIVAL        A                 4 0
016200891106     C     A             CHAIN     GA26D3SF                           21
016300000000     C  N21S1DES         IFNE      *BLANKS
016500011129     C                   Z-ADD     notnot        parnot
016501011129     C                   Z-ADD     A             NOTPRG
0166000000001    C                   MOVEL     S1DES         NOTDES
016700011210     C                   WRITE     mzNOT000
016800000000     C                   END
016900000000     C  N21              END
017000000000     C                   ENDSR
017201011129     C******************************************************
017400000000     C* ROLLUP    AGGIUNGE RIGHE VUOTE
017401011129     C******************************************************
017600000000     C     ROLLUP        BEGSR
017700000000     C                   MOVE      *BLANK        S1DES
017800000000     C                   Z-ADD     SAVNRR        NRR
017900000000     C     SAVNRR        DIV       14            F                 3 0
018000000000     C                   MVR                     RESTO             2 0
018100000000     C                   ADD       1             RESTO
018200000000     C     RESTO         DO        14            RESTO
018300000000     C                   ADD       1             NRR
0184000000007    C                   WRITE     GA26D3SF
018500000000     C                   END
018600000000     C                   Z-ADD     NRR           SAVNRR            4 0
018700000000     C                   Z-ADD     NRR           RNN               4 0
018800000000     C                   ENDSR
018901011129     C******************************************************
019000011129     C     *INZSR        BEGSR
019001011129     C******************************************************
019100011129     C* POSIZIONAMENTO N.RIFERIMENTO                       *
019200011129     C     KNOT          KLIST                                                  *
019300011129     C                   KFLD                    partip
019400011129     C                   KFLD                    parnot
019500011129     C     *ENTRY        PLIST                                                  *
019600011129     C                   PARM                    KPJBA                          *
019700011129     C                   Z-ADD     1             CODUT
019800011129     C                   CALL      'XPARUT'
019900011129     C                   PARM                    PARUT
020000011129     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
020100011129     C     CODUT         IFEQ      -1
020200011129     C                   SETON                                        LR
020300011129     C                   END
020400011129     C                   MOVEL     RAGUT         RSUT             20
020500011129     C                   MOVEL     KPJBU         PARAM
020600011129     I*--- NUNERO NOTE AGGIUNTIVE
020700011210     C     *DTAARA       DEFINE                  mzNOT             7 0
020701011221     C* visualizza
020702011221     c     parmod        comp      '5'                                    20
020800011129     C* ANNULLA
020900011129     C     PARMOD        IFEQ      '4'
021000011129     C                   EXSR      ANNUL
021001011129     c                   movel     'F'           parret
021100011129     C                   SETON                                        LR
021101011129     c                   else
021102011129     c                   movel     parnrf        vnrf
021103011129     c     parnrf        chain     mzame01l
021104011129     c                   if        %found(mzame01l)
021105011129     c                   movel     ametga        vtga
021106011129     c                   movel     ametgn        vtgn
021107011129     c                   movel     amemar        vmar
021108011129     c                   movel     ametip        vtip
021109011129     c                   movel     amedit        vdit
021200011129     C                   END
021201011129     C                   END
021300011129     C                   ENDSR
