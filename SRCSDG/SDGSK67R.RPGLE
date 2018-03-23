000100990426     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200990426     H*PARMS BNDDIR(PJXBND PJCBND)
000300990426     H*PARMS CVTOPT(*NONE)
000400990426     H DECEDIT('0,') DATEDIT(*DMY.)
000500990426      *
000600170303     >*            Lancio stampa movimenti percipienti CU (quadro AU)
000700170303      *           ---------------------------------------------------
000800170303      *
000900990426      *
001000990426      * DOCUMENTI MICROANALISI :
001100990426      *
001200990426      * NOTE TECNICHE :
001300990426      *
001400990426      *
001500990426      * PERSONALIZZAZIONI
001600990426      *
001700170303R2525 * Task di creazione: R252
001800990426      *
001900990426      *               MAPPA INDICATORI
002000990426      *
002100990426      *  21           GENERICO OPERAZIONI I/O
002200990426      *  22           GENERICO ERRORE OPERAZIONI I/O
002300990426      *  30           SFLDSP
002400990426      * N31           SFLCLR
002500990426      *  31           SFLDSPCTL
002600990426      *  32           SFLNXTCHG
002700990426      *  33           SFLEND
002800990426      *  39           OF PRTF
002900990426      *  40 <---> 49  DSPATR ERRORI SU SFL
003000990426      *  50 <---> 89  ERROSI SU VIDEO
003100990426      *  elenco errori speciali:
003200990426      *  90           REGISTRAZIONE GIA' ESISTENTE PER IMMISSIONE O COPIA
003300990426      *  91           OPZIONE NON VALIDA
003400990426      *  92           IMPOSSIBILE RIPOSIZIONAMENTO CON OPZIONI PRESENTI SU LISTA
003500990426      *  93           LA FUNZIONE RICHIAMATA SI E' CHIUSA IN ERRORE
003600990426      *  95           L'OPERAZIONE DI SCRITTURA NON E' ANDATA A BUON FINE
003700990426      *  94           UTENTE NON AUTORIZZATO ALLA FUNZIONE NEL MODO DESIDERATO
003800990426      *  96           ESISTONO ERRORI IN ALTRI SCHERMI
003900990426      *  97           TASTO   NON ABILITATO
004000990426      *  98           RICERCA NON ABIL. NELLA POSIZ.
004100990426      *  99           INDIC. GENERALE DI ERRORE
004200990426     F*----------------------------------------------------*
004300170302     FSDGSK67D  CF   E             WORKSTN
004400990426     F*-
004500990426     F*----------------------------------------------------*
004600000000     D KPJBA         E DS
004700990426     D*------------
004800990426     D ndcr70ds      E DS
004900990426     D*------------
005000990426     D* Dati di ambiente ottenuti da XSOC
005100990426     D SOC001        E DS                  EXTNAME(XSOC001DS)
005200990426     D*-------------
005300990426     D* DS Interna per dati di output di XSOC
005400990426     D XSOCDS          DS          1000
005500000000     I*----------------------------------------------------*
005600990426     D* Reperimento nome PGM
005700990426     D STATUS         SDS           333
005800990426     D  DSPGM            *PROC
005900990426     D  PARMS            *PARMS
006000990426     I*----------------------------------------------------*
006100990426     C*
006200990426     C                   Exsr      Inzvar                                       *
006300170302     C*
006500990426     C                   Z-Add     0             D1Anno                         *
006602170302     C                   Movel     'AU'          D1Qua                          *
006700990426     C*
006800170302     C                   Write     SK67T1                                       *
006801170302     C*
006900170302     C                   Do        *hival                                       *
006901170302     C*
007000170302     C                   Exfmt     SK67D1                                       *
007102170302     C*                                                    *
007200170302     C   KC              Leave                                                  *
007300170302     C   KN              Leave                                                  *
007400990426     C*
007500170302     C                   setoff                                       995152
007600990426     C     D1Anno        IfEq      0
007700990426     C                   seton                                        5199
007800990426     C                   EndIf
007901170302     C     D1Qua         IfNe      'AU'
008100990426     C                   SetOn                                        5299
008200990426     C                   EndIf
008300990426     C*
008400990426     C  NKF
008500170302     COR 99              Iter
008800990426     C*
008900990426     C                   Clear                   NdCR70DS                       *
009000990426     C                   MOVEL     XscSoc        SOCR70                         *
009100990426     C                   Z-Add     D1Anno        ANNOR70
009200020415     C*
009300170302     C*     Stampa quadro AU
009301170302     C*     percipienti
009400170302     C                   IF        D1QUA     =   'AU'
009500020415     C                   MOVE      *On           FlagDR70                       *
009600020415     C                   MOVE      *On           FlagD1R70                      *
009700020415     C                   MOVEL     NdCR70DS      KPJBU                          *
009800170302     C                   CALL      'NDCR72R1'                                   *
009900020415     C                   PARM                    KPJBA                          *
010000020415     C                   EndIf                                                  *
011000990426     C*
011100170302     C                   Iter                                                   *
011101170302     C*
011102170302     C                   Enddo
011200990426     C*
011400990426     C                   Exsr      EndPgm                                       *
011500990426     C************************************************************
011600990426     C* FINE PROGRAMMA
011700990426     C************************************************************
011800990426     C     ENDPGM        BEGSR
011900990426     C*
012000990426     C                   SETON                                            LR
012100990426     C*
012200990426     C                   ENDSR
012300990426     C/EJECT
012400990426     C************************************************************
012500990426     C* OPERAZIONI INIZIALI
012600990426     C************************************************************
012700990426     C     *INZSR        BEGSR
012800990426     C*
012900990426     C* Reperimento parametri
013000990426     C*
013100990426     C     *ENTRY        PLIST
013200990426     C                   PARM                    KPJBA
013300990426     C*
013400990426     C* Reperimento dati società
013500990426     C*
013600990426     C                   MOVEL     'SOC001'      TIPXSC
013700990426     C                   MOVEL     *BLANK        SOCXSC
013800990426     C                   EXSR      REPSOC
013900990426     C*
014000990426     C* Controlli dati società
014100990426     C*
014200990426     C     RTNXSC        IFNE      '1'
014300990426     C                   MOVEL     XSOCDS        SOC001
014400990426     C                   ELSE
014500990426     C                   EXSR      ENDPGM
014600990426     C                   ENDIF
014700990426     C*
014800990426     C                   ENDSR
014900990426     C/EJECT
015000990426     C************************************************************
015100990426     C* INIZIALIZZAZIONE VARIABILI
015200990426     C************************************************************
015300990426     C     INZVAR        BEGSR
015400990426     C*
015500990426     C* Valorizzazione campi univoci testate
015600990426     C*
015700170302     C                   CLEAR                   SK67T1
015800990426     C                   MOVEL     KNSIF         NOMSIF
015900990426     C                   MOVEL     XSCDSI        NOMDIT
016000990426     C                   MOVEL     DSPGM         NOMPGM
016100990426     C*
016200990426     C                   ENDSR
016300990426     C/EJECT
016400990426     C************************************************************
016500990426     C* REPERIMENTO DATI SOCIETA'
016600990426     C************************************************************
016700990426     C     REPSOC        BEGSR
016800990426     C*
016900990426     C                   CALLB     'XSOC'
017000990426     C                   PARM                    TIPXSC            6
017100990426     C                   PARM                    SOCXSC            3
017200990426     C                   PARM                    CDSXSC            9 0
017300990426     C                   PARM                    MODXSC            3
017400990426     C                   PARM      *BLANKS       RTNXSC            1
017500990426     C                   PARM                    XSOCDS
017600990426     C                   PARM                    KPJBA
017700990426     C*
017800990426     C                   ENDSR
017900990426     C/EJECT
