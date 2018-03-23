000100990118     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990118     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850201     F* ANG890R  *-AN/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500850201     F*       - INTERROGAZ.  ANAGRAFICO MATERIE PRIME -              *
000600000000     F*       -----------------------------------------              *
000700850201     F*                                                   AZN.: G890 *
000800990118     F*         FILE : tmAMP01L                                      *
000900000000     F*                                                              *
001000000000     F*         PGM. : RIC001R   - RICERCA INTESTAZIONE DITTA        *
001100000000     F*                                                              *
001200000000     F*--------------------------------------------------------------*
001300990118     FtmAMP01L  IF   E           K DISK
001400990317     Ftii008D   CF   E             WORKSTN
001500000000     F*--------------------------------------------------------------*
001600000000     D KPJBA         E DS
001700000000     D*-----PASSAGGIO PARAMETRI
001800871012     D  CODART               247    258
001900871012     D  DESART               259    288
002000850201     D  MSGERR               452    501
002100850201     D  RICHIA               502    502
002200990118     D soc001        e ds                  extname(xsoc001ds)
002300990118     D xsocds          DS          1000
002400990118     d dataiso         s               d   datfmt(*iso)
002500990118     d dataeur         s               d   datfmt(*eur)
002600000000     C*----------------------------------------------------*
002700000000     C     *ENTRY        PLIST                                                  *
002800000000     C                   PARM                    KPJBA                          *
002900990118     C*---------- RICERCA DITTA :
003000990118     C                   MOVEL     'SOC001'      TIPXSC
003100990118     C                   MOVEL     *blank        SOCXSC
003200990118     C                   EXSR      REPSOC
003300990118     C     RTNXSC        IFNE      '1'
003400990118     C                   MOVEL     XSOCDS        SOC001
003500990118     C                   MOVEL     xscrgs        desdit
003600990118     c                   end
003700990118     C                   MOVEL     *blank        richia            1
003800000000     C*---------- KEY PER RICERCA ARTICOLO :               *
003900000000     C     KEYART        KLIST                                                  *
004000000000     C                   KFLD                    CODART                         *
004100000000     C*     EMISSIONE PROMPT                               *
004200871012     C                   MOVEL     *BLANKS       INTERR           12
004300861211     C                   MOVEL     '?'           INTERR
004400000000     C*                                                    *
004500850201     C     RICHIA        CABEQ     'R'           INIZIO                   09    *
004600850201     C     SOPRA         TAG                                                    *
004700861211     C                   MOVEL     INTERR        CODART                         *
004800850201     C                   MOVE      *BLANKS       DESART                         *
004900000000     C     INIZIO        TAG                                                    *
005000850201     C                   WRITE     ANG8901                                      *
005100850201     C   91              SETON                                            90    * ART. ERRATO
005200850201     C                   EXFMT     ANG8902                                      *
005300850201     C                   MOVEA     *ZEROS        *IN(90)                        *
005400990409     C   Kc              GOTO      FINE                                         *
005500850201     C*---------- RICERCA ALFABETICA ARTICOLO :            *
005600861211     C     CODART        IFEQ      INTERR                                       *
005700000000     C                   MOVE      *BLANKS       CODART                         *
005800990317     C                   CALL      'TII104R'                                    *
005900871012     C                   PARM                    CODART                         *
006000000000     C                   PARM                    DESART                         *
006100000000     C                   GOTO      INIZIO                                       *
006200000000     C                   END                                                    *
006300000000     C*----------                                          *
006400990118     C     KEYART        CHAIN     tmAMP01L                           91        *
006500850201     C   91              GOTO      INIZIO                                       *
006600850201     C*----------ROVESCIO DATA INIZIO GESTIONE :           *
006700990118     C     AMPDIG        IFgt      0                                            *
006800990118     C                   move      AMPDIG        DATAiso                        *
006900990118     C                   move      dataiso       dataeur                        *
007000990118     C                   move      DATAeur       AMPDIG                         *
007100850201     C                   END                                                    *
007200850201     C*----------ROVESCIO DATA FINE GESTIONE :             *
007300990118     C     AMPDFG        IFgt      0                                            *
007400990118     C                   move      AMPDFG        DATAIso                        *
007500990118     C                   move      dataiso       dataeur                        *
007600990118     C                   move      DATAeur       AMPDFG                         *
007700850201     C                   END                                                    *
007800990118     C     AMPtar        IFgt      0                                            *
007900990118     C                   move      AMPTAR        dataiso
008000990118     C                   move      dataiso       dataeur                        *
008100990118     C                   move      DATAeur       AMPDFG                         *
008200990118     C                   end
008300990409     c                   do        *hival
008400850201     C                   EXFMT     ANG8903                                      *
008500990409     C   kl              GOTO      SOPRA                                        *
008600990409     c                   enddo
008700000000     C*     USCITA PROGRAMMA                               *
008800000000     C     FINE          TAG                                                    *
008900850201     C   99              MOVEL     'KPJB206'     KMSMN                          *
009000000000     C                   SETON                                            LR    *
009100990118     C*----------------------------------------------------*
009200990118     C* Reperimento dati società
009300990118     C*----------------------------------------------------*
009400990118     C     REPSOC        BEGSR
009500990118     C*
009600990118     C                   CALLB     'XSOC'
009700990118     C                   PARM                    TIPXSC            6
009800990118     C                   PARM                    SOCXSC            3
009900990118     C                   PARM                    CDSXSC            9 0
010000990118     C                   PARM                    MODXSC            3
010100990118     C                   PARM      *blanks       RTNXSC            1
010200990118     C                   PARM                    XSOCDS
010300990118     C                   PARM                    KPJBA
010400990118     C*
010500990118     C                   ENDSR
