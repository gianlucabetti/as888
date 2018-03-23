000100961218     H DECEDIT('0,') DATEDIT(*DMY.)
000200970122     H* TNTA33R *-----------------------------------------------------*
000300961224     H*               CONVALIDA DATI TIPO CLIENTE                     *
000400961219     H*---------------------------------------------------------------*
000500011002     FTEETC01L  UF   E           K DISK
000600011002     FTEETS01L  UF   E           K DISK
000601061009     FTEETS11L  UF   E           K DISK    rename(teets000:teets001)
000700061009     FTEETT01L  UF   E           K DISK
000800961218     D*
000900961218     D* PARAMETRI RICEVUTI DAL PGM CHIAMANTE
001000970122     D DSTA33        E DS                  EXTNAME(TNTA33DS)
001100961218     D*
001200961218     D KPJBA         E DS
001300961218     I/SPACE 3
001400961218     C*****************************************************************
001500961218     C* RIEPILOGO INDICATORI
001600961218     C*****************************************************************
001700961219     C* 30/33 - DI COMODO
001800961224     C* 39    - DELETE
001900961218     C*****************************************************************
002000961218     C     *ENTRY        PLIST
002100961218     C                   PARM                    KPJBA
002200970122     C                   MOVEL     KPJBU         DSTA33
002300961218     C*---------------------------------------------------------------*
002400970122     C* TIPO LANCIO: D33TLA = "C" -           CHIUDO CON LR
002500970122     C*              D33TLA = "L" - ELABORO E CHIUDO CON LR
002600970122     C*              D33TLA = " " - ELABORO E CHIUDO IN  RETRN
002700970122    0C     D33TLA        IFNE      'C'
002800961218     C*
002900961224     C***  C R E O    T N E T C
003000970409     C* SE ESISTE GIA' LO CANCELLO
003100970409     C     *IN30         DOUEQ     *ON
003200011002     C     KETC2         DELETE    TEETC01L                           30
003300970409     C                   ENDDO
003400970409     C**
003500011002     C     KETC          SETLL     TEETC01L
003600011002     C     KETC          READE     TEETC01L                               30
003700961220     C*
003800961224    1C     *IN30         DOWEQ     *OFF
003900970409     C     ETCATB        IFEQ      ' '
004000961224     C                   CLEAR                   ETCFTR
004100961224     C                   CLEAR                   ETCDTR
004200970122     C                   Z-ADD     D33KSC        ETCKSC
004300970122     C                   Z-ADD     D33PRG        ETCPRG
004400061009     C                   MOVEL     'S'           ETCDSF
004500970114     C                   MOVEL     'T'           ETCCTO
004600011002     C                   UPDATE    TEETC000
004700970409     C                   ELSE
004800011002     C                   DELETE    TEETC000
004900970409     C                   ENDIF
005000970117     C*
005100011002     C     KETC          READE     TEETC01L                               30
005200961224    1C                   ENDDO
005300961220     C*
005400061009     C***  C R E O    T N E T S 0 0 F
005500970409     C* SE ESISTE GIA' LO CANCELLO
005600970409     C     *IN30         DOUEQ     *ON
005700011002     C     KETC2         DELETE    TEETS01L                           30
005800970409     C                   ENDDO
005900970409     C**
006000011002     C     KETC          SETLL     TEETS01L
006100011002     C     KETC          READE     TEETS01L                               30
006200961224     C*
006300961224    1C     *IN30         DOWEQ     *OFF
006400970409     C     ETSATB        IFEQ      ' '
006500961224     C                   CLEAR                   ETSFTR
006600961224     C                   CLEAR                   ETSDTR
006700970122     C                   Z-ADD     D33KSC        ETSKSC
006800970122     C                   Z-ADD     D33PRG        ETSPRG
006900061009     C                   MOVEL     'S'           ETSDSF
007000970114     C                   MOVEL     'T'           ETSCTO
007100011002     C                   UPDATE    TEETS000
007200970409     C                   ELSE
007300011002     C                   DELETE    TEETS000
007400970409     C                   ENDIF
007500970117     C*
007600011002     C     KETC          READE     TEETS01L                               30
007700961224    1C                   ENDDO
007701061009     C*
007702061009     C***  C R E O    T N E T S 1 0 F
007703061009     C* SE ESISTE GIA' LO CANCELLO
007704061009     C     *IN30         DOUEQ     *ON
007705061009     C     KETC2         DELETE    TEETS11L                           30
007706061009     C                   ENDDO
007707061009     C**
007708061009     C     KETC          SETLL     TEETS11L
007709061009     C     KETC          READE     TEETS11L                               30
007710061009     C*
007711061009    1C     *IN30         DOWEQ     *OFF
007712061009     C     ETSATB        IFEQ      ' '
007713061009     C                   CLEAR                   ETSFTR
007714061009     C                   CLEAR                   ETSDTR
007715061009     C                   Z-ADD     D33KSC        ETSKSC
007716061009     C                   Z-ADD     D33PRG        ETSPRG
007717061009     C                   MOVEL     'S'           ETSDSF
007718061009     C                   MOVEL     'T'           ETSCTO
007719061009     C                   UPDATE    TEETS001
007720061009     C                   ELSE
007721061009     C                   DELETE    TEETS001
007722061009     C                   ENDIF
007723061009     C*
007724061009     C     KETC          READE     TEETS11L                               30
007725061009    1C                   ENDDO
007800961224     C*
007900961224     C***  C R E O    T N E T T
008000970409     C* SE ESISTE GIA' LO CANCELLO
008100970409     C     *IN30         DOUEQ     *ON
008200011002     C     KETC2         DELETE    TEETT01L                           30
008300970409     C                   ENDDO
008400970409     C**
008500011002     C     KETC          SETLL     TEETT01L
008600011002     C     KETC          READE     TEETT01L                               30
008700961224     C*
008800961224    1C     *IN30         DOWEQ     *OFF
008900970409     C     ETTATB        IFEQ      ' '
009000961224     C                   CLEAR                   ETTFTR
009100961224     C                   CLEAR                   ETTDTR
009200970122     C                   Z-ADD     D33KSC        ETTKSC
009300970122     C                   Z-ADD     D33PRG        ETTPRG
009400061009     C                   MOVEL     'S'           ETTDSF
009500970114     C                   MOVEL     'T'           ETTCTO
009600011002     C                   UPDATE    TEETT000
009700970409     C                   ELSE
009800011002     C                   DELETE    TEETT000
009900970409     C                   ENDIF
010000961224     C*
010100011002     C     KETC          READE     TEETT01L                               30
010200961224    1C                   ENDDO
010300961224     C*
010400961224    0C                   ENDIF
010500961224     C*
010600961224     C* CHIUSURA PGM
010700970122    0C     D33TLA        IFEQ      ' '
010800961224     C                   RETURN
010900961224   X0C                   ELSE
011000961224     C                   SETON                                        LR
011100961224    0C                   ENDIF
011200961219     C*
011300961218     C*--- SR INIZIALE -----------------------------------------------*
011400961218     C     *INZSR        BEGSR
011500961218     C****
011600961218     C** KLIST
011700961218     C****
011800011002     C* ACCESSO TEETC01L / TEETS01L / TEETT01L
011900961224     C     KETC          KLIST
012000980526     C                   KFLD                    WDSF
012100980526     C                   KFLD                    WOFF
012200970122     C                   KFLD                    D33NRV
012300970122     C                   KFLD                    D33CTR
012400970409     C                   KFLD                    WZERO
012500011002     C* ACCESSO TEETC01L
012600961224     C     KETC2         KLIST
012700980526     C                   KFLD                    WDSF
012800970114     C                   KFLD                    WTAR
012900970122     C                   KFLD                    D33KSC
013000970122     C                   KFLD                    D33CTR
013100970122     C                   KFLD                    D33PRG
013200961224     C*
013300961218     C* IMPOSTAZIONE CAMPI FISSI
013400961224     C                   CLEAR                   WZERO
013500061009     C                   MOVEL     'S'           WDSF
013600980526     C                   MOVEL     'T'           WTAR
013700970114     C                   MOVEL     'O'           WOFF
013800961218     C*
013900961218     C****
014000961218     C** DEFINIZIONE CAMPI
014100961218     C****
014200961224     C     *LIKE         DEFINE    ETCPRG        WZERO
014300980526     C     *LIKE         DEFINE    ETCDSF        WDSF
014400980526     C     *LIKE         DEFINE    ETCCTO        WOFF
014500970114     C     *LIKE         DEFINE    ETCCTO        WTAR
014600961218     C*
014700961218     C                   ENDSR
014800961218     C*
