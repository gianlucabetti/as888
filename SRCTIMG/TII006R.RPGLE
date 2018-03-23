000100990118     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990118     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F* ANG820R  *-AN/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500000000     F*       - INTERROGAZ.  FORNITORI PARTICOLARITA'      -         *
000600000000     F*       ----------------------------------------------         *
000700000000     F*                                                   AZN.: G820 *
000800990118     F*         FILE : ANFOP01L - tmFOP00F - ANTAB00F                *
000900990118     F*                           anfrn00F - ANG820D                 *
001000000000     F*                                                              *
001100990317     F*         PGM. : tii103R   - RICERCA RAG.SOC.USO INTERNO       *
001200000000     F*                                                              *
001300000000     F*--------------------------------------------------------------*
001400990118     FtmFOP01L  IF   E           K DISK
001500860521     F****************************************************************
001600990118RIC  Fanfrn01l  IF   E           K DISK
001700990118RIC  Fansog01l  IF   E           K DISK
001800990412     Fancdp01l  IF   E           K DISK
001900860521     F****************************************************************
002000000000     FANTAB00F  IF   E           K DISK
002100990317     Ftii006D   CF   E             WORKSTN
002200000000     F*--------------------------------------------------------------*
002300000000     D                 DS
002400000000     D*---- COMMENTI E INFORMAZIONI FORNITORE
002500000000     D  VIDIN1                 1     50
002600000000     D  VIDIN2                51    100
002700000000     D  VIDIN3               101    150
002800000000     D  VIDIN4               151    200
002900000000     D  VIDINF                 1    200
003000860521     D KPJBA         E DS
003100990118     D soc001        E DS                  EXTNAME(xsoc001ds)
003200990118     D xsocds          DS          1000
003300000000     C*----------------------------------------------------*
003400000000     C     *ENTRY        PLIST                                                  *
003500000000     C                   PARM                    KPJBA                          *
003600990118     C*---------- RICERCA DITTA :
003700990118     C                   MOVEL     'SOC001'      TIPXSC
003800990118     C                   MOVEL     *blank        SOCXSC
003900990118     C                   EXSR      REPSOC
004000990118     C     RTNXSC        IFNE      '1'
004100990118     C                   MOVEL     XSOCDS        SOC001
004200990118     C                   MOVEL     xscrgs        RSUT             20
004300990118     C                   MOVEL     xscrgs        desdit
004400990118     c                   movel     xscsoc        wsoc
004500990118     c                   end
004600990118     c                   movel     *blank        richia            1
004700990118     C*                                                    *
004800990118     c     kfrn          klist
004900990118     c                   kfld                    wsoc              3
005000990118     c                   kfld                    codfor
005100990412     c     kcdp          klist
005200990412     c                   kfld                    wsoc
005300990412     c                   kfld                    frncondpag
005400000000     C*-- KEY PER RICERCA TABELLE SU ANTAB00F :            *
005500000000     C     KEYANT        KLIST                                                  *
005600000000     C                   KFLD                    TABGRU                         *
005700000000     C                   KFLD                    TABCOD                         *
005800860521     C******************************************************
005900860521     C*                                                    *
006000860521     C******************************************************
006100000000     C/EJECT
006200860521     C*--------- EMISSIONE PROMPT  ------------------------*
006300000000     C*                                                    *
006400841211     C     RICHIA        CABEQ     'R'           INIZIO                   09    *
006500990118     C                   MOVE      '?       '    CODFOR                         *
006600000000     C                   MOVE      *BLANKS       DESFOR                         *
006700000000     C     INIZIO        TAG                                                    *
006800841211     C                   WRITE     ANG8201                                      *
006900000000     C   91                                                                     *
007000000000     COR 92              SETON                                            90    *
007100000000     C                   EXFMT     ANG8202                                      *
007200841211     C                   MOVEA     *ZEROS        *IN(11)                        *
007300990408     C   Kc              GOTO      FINE                                         *
007400000000     C*---------- CONTROLLO DATI INPUT :                   *
007500000000     C     CODFOR        CABEQ     *BLANKS       INIZIO                   91    *
007600000000     C*---------- RICERCA ALFABETICA :                     *
007700990408     c                   if        codfor = '?      ' or codfor = '0000000?'
007800990408     C*    CODFOR        IFEQ      '?       '                                   *
007900000000     C                   MOVE      *BLANKS       CODFOR                         *
008000990317     C                   CALL      'TII103R'                                    *
008100000000     C                   PARM                    CODFOR                         *
008200000000     C                   PARM                    DESFOR                         *
008300000000     C                   GOTO      INIZIO                                       *
008400000000     C                   END                                                    *
008500000000     C**--- CONTROLLO PRESENZA FORNITORE SUI FILES ------***
008600990118     C     CODFOR        CHAIN     tmFOP01L                           10        *
008700000000     C     *IN(10)       CABEQ     '1'           INIZIO                   92    *
008800000000     C                   EXSR      RUVIVE                                       * VAR. FORNIT.
008900990118     C*ricerca sui fornitori                               *
009000990118RIC  C     Kfrn          CHAIN     anfrn01l                           15        *
009100990118RIC  C   15frnsogg       CHAIN     ansog01l                           15        *
009200990412RIC  C  n15Kcdp          CHAIN     ancdp01l                           15        *
009300990118RIC  C  N15              MOVEl     sogdes        VIDRAG                         *
009400990118RIC  C  N15              MOVE      frnKCC        VIDCFO                         *
009500990118     C  N15              MOVE      frncondpag    VIDCps
009600990412     C  n15              MOVEl     cdpdescr      descps
009700990118     C   15              MOVE      *BLANKS       VIDCps
009800990118     C   15              MOVE      *BLANKS       VIDrag
009900860521ADB  C*--- RICERCA RECORD ANFOR01L                         * PERS
010000860521     C******************************************************
010100000000     C                   EXSR      RUTCTL                                       * PREPAR.DATI
010200000000     C*--- EMISSIONE VIDEATA DI INTERROGAZIONE             *
010300990408     c                   do        *hival
010400000000     C                   EXFMT     ANG8203                                      *
010500990408     C   kl              GOTO      INIZIO                                       *
010600990408     c                   enddo
010700000000     C*----------------------------------------------------*
010800841211     C   09              MOVE      VIDRSI        DESFOR                         *
010900000000     C     FINE          TAG                                                    *
011000000000     C                   SETON                                            LR    *
011100000000     C*--- PREPARA CAMPI DESCRIZIONI ----------------------*
011200000000     C***------------------------------------------------***
011300000000     CSR   RUTCTL        BEGSR                                                  *
011400000000     C*---------                                           * GRUP.MERC.
011500000000     C                   MOVE      '159'         TABGRU                         *
011600000000     C                   MOVE      *ZEROS        TABCOD                         *
011700000000     C                   MOVE      VIDGPM        TABCOD                         *
011800000000     C     KEYANT        CHAIN     ANTAB00F                           99        *
011900000000     C  N99              MOVEL     TABDES        DESGPM                         *
012000000000     C*---------                                           * SETTORE
012100000000     C                   MOVE      '160'         TABGRU                         *
012200000000     C                   MOVE      *ZEROS        TABCOD                         *
012300000000     C                   MOVE      VIDSTM        TABCOD                         *
012400000000     C     KEYANT        CHAIN     ANTAB00F                           99        *
012500000000     C  N99              MOVEL     TABDES        DESSTM                         *
012600860521     C****************************************************** PAGAMENTO
012700000000     C*---------                                           * LISTINO
012800000000     C                   MOVE      '161'         TABGRU                         *
012900000000     C                   MOVE      *ZEROS        TABCOD                         *
013000000000     C                   MOVE      VIDLIS        TABCOD                         *
013100000000     C     KEYANT        CHAIN     ANTAB00F                           99        *
013200000000     C  N99              MOVEL     TABDES        DESLIS                         *
013300000000     C*---------                                           * CLAS.APPROV.
013400000000     C                   MOVE      '157'         TABGRU                         *
013500000000     C                   MOVE      *ZEROS        TABCOD                         *
013600000000     C                   MOVE      VIDAPP        TABCOD                         *
013700000000     C     KEYANT        CHAIN     ANTAB00F                           99        *
013800000000     C  N99              MOVEL     TABDES        DESAPP                         *
013900000000     CSR                 ENDSR                                                  *
014000000000     C***------------------------------------------------***
014100000000     C/EJECT
014200000000     C***------------------------------------------------***
014300000000     C**--- IMPOSTAZIONE CAMPI VIDEO :-------------------***
014400000000     C***------------------------------------------------***
014500000000     CSR   RUVIVE        BEGSR                                                  *
014600000000     C                   MOVE      *BLANKS       VIDRAG                         *
014700000000     C                   MOVE      FOPCFO        VIDCFO                         *
014800000000     C                   MOVE      FOPRSI        VIDRSI                         *
014900000000     C                   MOVE      FOPGPM        VIDGPM                         *
015000000000     C                   MOVE      *BLANKS       DESGPM                         *
015100000000     C                   MOVE      FOPSTM        VIDSTM                         *
015200000000     C                   MOVE      *BLANKS       DESSTM                         *
015300000000     C                   MOVE      *BLANKS       DESCPS                         *
015400000000     C                   MOVE      FOPSC1        VIDSC1                         *
015500000000     C                   MOVE      FOPSC2        VIDSC2                         *
015600000000     C                   MOVE      FOPLIS        VIDLIS                         *
015700000000     C                   MOVE      *BLANKS       DESLIS                         *
015800000000     C                   MOVE      FOPAPP        VIDAPP                         *
015900000000     C                   MOVE      *BLANKS       DESAPP                         *
016000000000     C                   MOVE      FOPDCC        VIDDCC                         *
016100000000     C                   MOVE      FOPQFO        VIDQFO                         *
016200000000     C                   MOVE      FOPVAL        VIDVAL                         *
016300000000     C                   MOVE      FOPINF        VIDINF                         *
016400000000     CSR                 ENDSR                                                  *
016500990118     C*----------------------------------------------------*
016600990118     C* Reperimento dati società
016700990118     C*----------------------------------------------------*
016800990118     C     REPSOC        BEGSR
016900990118     C*
017000990118     C                   CALLB     'XSOC'
017100990118     C                   PARM                    TIPXSC            6
017200990118     C                   PARM                    SOCXSC            3
017300990118     C                   PARM                    CDSXSC            9 0
017400990118     C                   PARM                    MODXSC            3
017500990118     C                   PARM      *blanks       RTNXSC            1
017600990118     C                   PARM                    XSOCDS
017700990118     C                   PARM                    KPJBA
017800990118     C*
017900990118     C                   ENDSR
