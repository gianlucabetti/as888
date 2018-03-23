000100990115     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990115     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300870414     F* ANG830R  *-AN/38-*-------------------------------------------*
000400870414     F*------------------*                                           *
000500870414     F*       - INTERROGAZIONE CLIENTI PARTICOLARITA'      -         *
000600870414     F*       ----------------------------------------------         *
000700870414     F*                                                   AZN.: G830 *
000800870414     F*                                                              *
000900990317     F*         PGM. : TII119R   - RICERCA RAG.SOC.USO INTERNO       *
001000870414     F*                XPARCCC   - RICERCA PARAMETRI VARI            *
001100870414     F*                                                              *
001200870414     F*--------------------------------------------------------------*
001300990115     FtmCLP01L  IF   E           K DISK
001400870414     F*************************************************************
001500990115     Fancln01l  IF   E           K DISK
001600990115     Fansog01l  IF   E           K DISK
001700990115     Fanind01l  IF   E           K DISK
001800990412     Fancdp01l  IF   E           K DISK
001900870414     F*FTAB01LIF  E           K        DISK
002000870414     F*************************************************************
002100870414     FANTAB00F  IF   E           K DISK
002200990317     Ftii007D   CF   E             WORKSTN
002300870414     F*--------------------------------------------------------------*
002400870414     D****************************************************************
002500870414RIC  D PAGAM           DS
002600870414RIC  D*---- KEY PAGAMENTI PER TABEL00F
002700870414RIC  D  VIDCPS                 1      3
002800870414RIC  D  LINGUA                 4      4
002900870414RIC  D  BLANCO                 5      8
003000870414RIC  D  KEYTBL                 1      8
003100870414     D****************************************************************
003200870414     D                 DS
003300870414     D*---- COMMENTI E INFORMAZIONI FORNITORE
003400870414     D  VIDIN1                 1     50
003500870414     D  VIDIN2                51    100
003600870414     D  VIDIN3               101    150
003700870414     D  VIDIN4               151    200
003800870414     D  VIDINF                 1    200
003900870414     D KPJBA         E DS
004000990115     D soc001        E DS                  EXTNAME(xsoc001ds)
004100990115     D xsocds          DS          1000
004200870414     C*----------------------------------------------------*
004300870414     C     *ENTRY        PLIST                                                  *
004400870414     C                   PARM                    KPJBA                          *
004500990115     C*---------- RICERCA DITTA :
004600990115     C                   MOVEL     'SOC001'      TIPXSC
004700990115     C                   MOVEL     *blank        SOCXSC
004800990115     C                   EXSR      REPSOC
004900990115     C     RTNXSC        IFNE      '1'
005000990115     C                   MOVEL     XSOCDS        SOC001
005100990115     C                   MOVEL     xscrgs        RSUT             20
005200990115     C                   MOVEL     xscrgs        desdit
005300990115     c                   movel     xscsoc        wsoc
005400990115     c                   end
005500990115     C*                                                    *
005600990115     c     kcln          klist
005700990115     c                   kfld                    wsoc              3
005800990118     c                   kfld                    codcli
005900990412     c     kcdp          klist
006000990412     c                   kfld                    wsoc
006100990412     c                   kfld                    clncondpag
006200870414     C******************************************************
006300870414     C*-- KEY PER RICERCA TABELLE SU ANTAB00F :            *
006400870414     C     KEYANT        KLIST                                                  *
006500870414     C                   KFLD                    TABGRU                         *
006600870414     C                   KFLD                    TABCOD                         *
006700870414     C******************************************************
006800990115     c                   move      *blank        richia            1
006900870414     C*--------- EMISSIONE PROMPT -------------------------*
007000870414     C*                                                    *
007100870414     C     RICHIA        CABEQ     'R'           INIZIO                   09    *
007200990115     C                   MOVE      '?       '    CODCLI            8            *
007300990115     C                   MOVE      *BLANKS       DESCLI           30            *
007400870414     C     INIZIO        TAG                                                    *
007500870414     C                   WRITE     ANG830D1                                     *
007600870414     C   91                                                                     *
007700870414     COR 92              SETON                                            90    *
007800870414     C                   EXFMT     ANG830D2                                     *
007900870414     C                   MOVEA     *ZEROS        *IN(11)                        *
008000990408     C   Kc              GOTO      FINE                                         *
008100870414     C*---------- CONTROLLO DATI INPUT :                   *
008200870414     C     CODCLI        CABEQ     *BLANKS       INIZIO                   91    *
008300870414     C*---------- RICERCA ALFABETICA :                     *
008400990408     c                   if        codcli = '?      ' or codcli = '0000000?'
008500990408     C*    CODCLI        IFEQ      '?       '                                   *
008600870414     C                   MOVE      *BLANKS       CODCLI                         *
008700990317     C                   CALL      'TII119R'                                    *
008800870414     C                   PARM                    CODCLI                         *
008900870414     C                   PARM                    DESCLI                         *
009000870414     C                   GOTO      INIZIO                                       *
009100870414     C                   END                                                    *
009200870414     C*----------------------------------------------------*
009300870414     C/EJECT
009400870414     C**--- CONTROLLO PRESENZA CLIENTI   SUI FILES ------***
009500990115     C     codcli        CHAIN     tmCLP01L                           10        *
009600870414     C     *IN(10)       CABEQ     '1'           INIZIO                   92    *
009700870414     C                   EXSR      RUVIVE                                       * VAR. FORNIT.
009800870414     C******************************************************
009900990115RIC  C     Kcln          CHAIN     ancln01l                           15        *
010000990115RIC  C   15clnsogg       CHAIN     ansog01l                           15        *
010100990412RIC  C  n15kcdp          CHAIN     ancdp01l                           15        *
010200990115RIC  C  N15              MOVEl     sogdes        VIDRAG                         *
010300990115RIC  C  N15              MOVE      clnKCC        VIDCFO                         *
010400990115     C  N15              MOVE      clncondpag    VIDCDP
010500990412RIC  C  N15              MOVEL     cdpdescr      DESCPS                         *
010600990115     C   15              MOVE      *BLANKS       VIDCDP
010700990115     C   15              MOVE      *BLANKS       VIDrag
010800870414     C                   EXSR      RUTCTL                                       * CONTR. DATI
010900870414     C*--- EMISSIONE VIDEATA DI INTERROGAZIONE             *
011000990408     c                   do        *hival
011100870414     C                   EXFMT     ANG830D3                                     *
011200990408     C   kl              goto      inizio                                       *
011300990408     c                   enddo
011400870414     C*----------------------------------------------------*
011500870414     C   09              MOVE      VIDRSI        DESCLI                         *
011600870414     C     FINE          TAG                                                    *
011700870414     C                   SETON                                            LR    *
011800870414     C*----------------------------------------------------*
011900870414     C*--- PREPARA CAMPI DESCRIZIONI ----------------------*
012000870414     C***------------------------------------------------***
012100870414     CSR   RUVIVE        BEGSR                                                  *
012200870414     C                   MOVE      CLPRSI        VIDRSI                         *
012300870414     C                   MOVE      CLPKCC        VIDCFO                         *
012400870414     C                   MOVE      CLPGPM        VIDGPM                         *
012500870414     C                   MOVE      *BLANKS       DESGPM                         *
012600870414     C                   MOVE      CLPSTM        VIDSTM                         *
012700870414     C                   MOVE      *BLANKS       DESSTM                         *
012800870414     C                   MOVE      *BLANKS       DESCPS                         *
012900870414     C                   MOVE      CLPSC1        VIDSC1                         *
013000870414     C                   MOVE      CLPSC2        VIDSC2                         *
013100870414     C                   MOVE      CLPLIS        VIDLIS                         *
013200870414     C                   MOVE      *BLANKS       DESLIS                         *
013300870414     C                   MOVE      CLPRES        VIDRES                         *
013400870414     C                   MOVE      CLPQCL        VIDQCL                         *
013500870414     C                   MOVE      CLPINF        VIDINF                         *
013600870414     C                   MOVEL     CLPCDF        VIDCDF
013700870414     C                   MOVE      CLPIMB        VIDIMB
013800870414     C                   MOVE      CLPSPE        VIDSPE
013900870414     C                   MOVE      CLPPCL        VIDPCL
014000870414     CSR                 ENDSR                                                  *
014100870414     C***------------------------------------------------***
014200870414     C***------------------------------------------------***
014300870414     CSR   RUTCTL        BEGSR                                                  *
014400870414     C*---------                                           * RAG.SOC.
014500870414     C                   MOVE      '159'         TABGRU                         *
014600870414     C                   MOVE      *ZEROS        TABCOD                         *
014700870414     C                   MOVE      VIDGPM        TABCOD                         *
014800870414     C     KEYANT        CHAIN     ANTAB00F                           93        *
014900870414     C  N93              MOVEL     TABDES        DESGPM                         *
015000870414     C*---------                                           * SETTORE
015100870414     C                   MOVE      '160'         TABGRU                         *
015200870414     C                   MOVE      *ZEROS        TABCOD                         *
015300870414     C                   MOVE      VIDSTM        TABCOD                         *
015400870414     C     KEYANT        CHAIN     ANTAB00F                           94        *
015500870414     C  N94              MOVEL     TABDES        DESSTM                         *
015600870414     C*---------                                           * LISTINO
015700870414     C                   MOVE      '161'         TABGRU                         *
015800870414     C                   MOVE      *ZEROS        TABCOD                         *
015900870414     C                   MOVE      VIDLIS        TABCOD                         *
016000870414     C     KEYANT        CHAIN     ANTAB00F                           97        *
016100870414     C  N97              MOVEL     TABDES        DESLIS                         *
016200870414     C*---------                                           * CLAS.APPROV.
016300870414     CSR                 ENDSR                                                  *
016400990115     C*----------------------------------------------------*
016500990115     C* Reperimento dati società
016600990115     C*----------------------------------------------------*
016700990115     C     REPSOC        BEGSR
016800990115     C*
016900990115     C                   CALLB     'XSOC'
017000990115     C                   PARM                    TIPXSC            6
017100990115     C                   PARM                    SOCXSC            3
017200990115     C                   PARM                    CDSXSC            9 0
017300990115     C                   PARM                    MODXSC            3
017400990115     C                   PARM      *blanks       RTNXSC            1
017500990115     C                   PARM                    XSOCDS
017600990115     C                   PARM                    KPJBA
017700990115     C*
017800990115     C                   ENDSR
017900870414     C***------------------------------------------------***
