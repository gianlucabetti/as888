000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011128     F*----------------------------------------------------*
000300000000     F*          DESCRIZIONI AGGIUNTIVE MANUTENZIONI       *
000400000000     F*----------------------------------------------------*
000500011210     FmzNOT01L  UF A E           K DISK
000600011129     Fmzame01l  iF   E           K DISK
000700160518     FTntbe01l  IF   E           K DISK
000800011128     FTRMZ26D3  CF   E             WORKSTN
000900000000     F                                     SFILE(GA26D3SF:NRR)
001000000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
001100160518     D dmau          E DS
001200160518     D dcau          E DS
001300000000     D KPJBA         E DS
001400011128     D PARAM           DS
001500011128     D* numero nota
001600011129     D  ParNRF                        7  0
001700011129     D  Partip                        2
001800011129     D  ParNOT                        7  0
001900011128     c* modalità con cui chiamo il pgm
002000011129     D  parmod                        1
002100011128     c* Cmd di ritorno
002200011129     D  parret                        1
002300011129     c*
002400011129     C                   if        not *inlr
002500000000     C                   EXSR      PULIZ
002600000000     C* CARICAMENTO SUBFILE
002700000000     C                   EXSR      CARIC
002800891106     C                   WRITE     GA26D301
002900011129     c*
003000011128     C                   DO        *HIVAL
003100011221     c                   if        *in20
003200011221     c                   write     ga26d3sc
003300011221     c                   EXFMT     PROTECT
003400011221     C                   ELSE
003500000000     C                   EXFMT     GA26D3SC
003600011221     c                   end
003700011129     c* fine
003800011129     c                   if        *inkc
003900011129     c                   movel     'C'           parret
004000011129     c                   leave
004100011129     C                   END
004200011129     c* rollup
004300000000     C                   SETOFF                                           71
004400000000     C   60              EXSR      ROLLUP
004500000000     C   60              SETON                                            71
004600011129     c* conferma
004700011129     c                   if        *inkf
004800000000     C                   EXSR      ANNUL
004900000000     C                   EXSR      SCRIVI
005000011129     c                   movel     'F'           parret
005100011129     c                   leave
005200000000     C                   END
005300011129     C                   ENDdo
005400011129     C                   END
005500011129     c                   seton                                        lr
005600000000     C                   MOVEL     PARAM         KPJBU
005700000000     C******************************************************
005800000000     C** PULIZIA DEL SUBFILE
005900011129     C******************************************************
006000011129     C     PULIZ         BEGSR
006100000000     C                   SETON                                        70
006200000000     C                   SETOFF                                       71
006300891106     C                   WRITE     GA26D3SC
006400000000     C                   SETON                                        71
006500000000     C                   SETOFF                                       70
006600000000     C                   ENDSR
006700000000     C******************************************************
006800011129     C** CARICAMENTO SUBFILE
006900011129     C******************************************************
007000000000     C     CARIC         BEGSR
007100000000     C                   Z-ADD     0             NRR               4 0
007200011210     C     KNOT          SETLL     mzNOT000
007300011129     c                   do        *hival
007400011210     C     KNOT          READE     mzNOT000                               01
007500011129     C   01              leave
007600000000     C                   MOVEL     NOTDES        S1DES
007700000000     C                   ADD       1             NRR
007800000000     C                   WRITE     GA26D3SF
007900000000     C                   END
008000000000     C                   Z-ADD     NRR           SAVNRR
008100000000     C* CARICO RECORDS VUOTI
008200000000     C                   EXSR      ROLLUP
008300000000     C                   ENDSR
008400000000     C******************************************************
008500000000     C** ANNULLAMENTO DEI RECORD
008600011129     C******************************************************
008700011129     C     ANNUL         BEGSR
008800000000     C                   DO        *HIVAL
008900011210     C     KNOT          DELETE    mzNOT000                           21
009000000000     C  N21              END
009100000000     C                   ENDSR
009200011129     C******************************************************
009300000000     C** SCRITTURA DEI NUOVI
009400000000     C** RECORD
009500011129     C******************************************************
009600011129     C     SCRIVI        BEGSR
009700000000     C* VEDO SE HA AGGIUNTO   ALMENO UNA DESCRIZIONE
009800000000     C* SE SI' CREO NUOVO NUMERO
009900000000     C     PARNOT        IFEQ      0
010000891106     C                   READC     GA26D3SF                               21
010100000000     C     *IN21         DOWEQ     '0'
010200000000     C     S1DES         COMP      *BLANK                                 22    22
010300000000     C                   READC     GA26D3SF                               21
010400011129     C  N22              ENDdo
010500000000     C  N22              DO
010600011210     C     *LOCK         IN        mzNOT
010700011210     C                   ADD       1             mzNOT
010800011210     C                   OUT       mzNOT
010900011210     C                   Z-ADD     mzNOT         PARNOT
011000011129     C                   ENDdo
011100011129     C                   ENDif
011200011129     C*
011300000000     C                   Z-ADD     PARNOT        NOTNOT
011400011129     c                   z-add     0             parnot
011500000000      *
011600000000     C                   DO        *HIVAL        A                 4 0
011700891106     C     A             CHAIN     GA26D3SF                           21
011800000000     C  N21S1DES         IFNE      *BLANKS
011900011129     C                   Z-ADD     notnot        parnot
012000011129     C                   Z-ADD     A             NOTPRG
0121000000001    C                   MOVEL     S1DES         NOTDES
012200011210     C                   WRITE     mzNOT000
012300000000     C                   END
012400000000     C  N21              END
012500000000     C                   ENDSR
012600011129     C******************************************************
012700000000     C* ROLLUP    AGGIUNGE RIGHE VUOTE
012800011129     C******************************************************
012900000000     C     ROLLUP        BEGSR
013000000000     C                   MOVE      *BLANK        S1DES
013100000000     C                   Z-ADD     SAVNRR        NRR
013200000000     C     SAVNRR        DIV       14            F                 3 0
013300000000     C                   MVR                     RESTO             2 0
013400000000     C                   ADD       1             RESTO
013500000000     C     RESTO         DO        14            RESTO
013600000000     C                   ADD       1             NRR
0137000000007    C                   WRITE     GA26D3SF
013800000000     C                   END
013900000000     C                   Z-ADD     NRR           SAVNRR            4 0
014000000000     C                   Z-ADD     NRR           RNN               4 0
014100000000     C                   ENDSR
014200011129     C******************************************************
014300011129     C     *INZSR        BEGSR
014400011129     C******************************************************
014500011129     C* POSIZIONAMENTO N.RIFERIMENTO                       *
014600011129     C     KNOT          KLIST                                                  *
014700011129     C                   KFLD                    partip
014800011129     C                   KFLD                    parnot
014900160518      *
015000160518     C     KTbe          KLIST                                                  *
015100160518     C                   KFLD                    tbecod                         *
015200160518     C                   KFLD                    tbeke1                         *
015300160518      *
015400011129     C     *ENTRY        PLIST                                                  *
015500011129     C                   PARM                    KPJBA                          *
015600011129     C                   Z-ADD     1             CODUT
015700011129     C                   CALL      'XPARUT'
015800011129     C                   PARM                    PARUT
015900011129     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
016000011129     C     CODUT         IFEQ      -1
016100011129     C                   SETON                                        LR
016200011129     C                   END
016300011129     C                   MOVEL     RAGUT         RSUT             20
016400011129     C                   MOVEL     KPJBU         PARAM
016500011129     I*--- NUNERO NOTE AGGIUNTIVE
016600011210     C     *DTAARA       DEFINE                  mzNOT             7 0
016700011221     C* visualizza
016800011221     c     parmod        comp      '5'                                    20
016900011129     C* ANNULLA
017000011129     C     PARMOD        IFEQ      '4'
017100011129     C                   EXSR      ANNUL
017200011129     c                   movel     'F'           parret
017300011129     C                   SETON                                        LR
017400011129     c                   else
017500011129     c                   movel     parnrf        vnrf
017600011129     c     parnrf        chain     mzame01l
017700011129     c                   if        %found(mzame01l)
017800011129     c                   movel     ametga        vtga
017900011129     c                   movel     ametgn        vtgn
018000011129     c                   movel     amedit        vdit
018100160518     C                   MOVEL     AMEmau        VMAu
018200160518     C                   MOVEL     AMEcaut       Vcaut
018300160518     c                   movel     'MAU'         tbecod
018400160518     c                   movel(p)  amemau        tbeke1
018500160518     c     ktbe          chain     tntbe01l
018600160518     c                   if        %found(tntbe01l)
018700160518     c                   movel     tbeuni        dmau
018800160518     c                   movel     §maudes       vmaud
018900160518     c                   endif
019000160518     c                   movel     'CAU'         tbecod
019100160518     c                   movel(p)  amecaut       tbeke1
019200160518     c     ktbe          chain     tntbe01l
019300160518     c                   if        %found(tntbe01l)
019400160518     c                   movel     tbeuni        dcau
019500160518     c                   movel     §caudes       vcautd
019600160518     c                   endif
019700011129     C                   END
019800011129     C                   END
019900011129     C                   ENDSR
