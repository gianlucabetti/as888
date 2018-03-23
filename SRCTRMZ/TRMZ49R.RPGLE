000100950718     H*------------------------------------------------------------------------*
000200011219     H* RICERCA E SCELTA                                                       *
000300160805     H* PERMETTE LA RICERCA E LA SELEZIONE PER tessera telepas              O  *
000400011120     H*                                                                        *
000500011120     h* PARAMETRI:
000600160805     h* telepass       20
000700011219     h* numero rifer.   7,0O
000800011218     h* codice ritorno  1  O  (comando premuto)
000900950718     H*------------------------------------------------------------------------*
001000950718     H DECEDIT('0,') DATEDIT(*DMY.)
001100950718     F*------------------------------------------------------------------------*
001200950718     F* DATA BASE
001300950718     F*------------------------------------------------------------------------*
001400160805     FTRmz49D   CF   E             WORKSTN
001500011120     F                                     SFILE(SFL01:RRN1)
001600160805     Fmzame05l  IF   E           K DISK
001700011120     D WrkSort         S              1
001800011120     D VDESS           S                   LIKE(VDES)
001900011120     D rrn1            S              5i 0
002000011219     c* ordina il sfl per telaio
002100011120     C                   EXSR      Initialize
002200011121     c                   if        rrn1 = 0
002300011121     c                   seton                                        41
002400011121     c                   end
002500011120     c*
002600011120     C                   DO        *HIVAL
002700011121     c*
002800011121     C                   WRITE     TOP
002900011121     C                   WRITE     BOT
003000011120     C                   EXFMT     CTL01
003100011126     c                   if        *inkl
003200011126     c                   move      'L'           cmd
003300011126     C                   LEAVE
003400011126     c                   end
003500011121     c                   if        rrn1 = 0
003600011121     c                   iter
003700011121     c                   end
003800011121     c*Controlli
003900011120     c                   exsr      ctr
004000011121     c*effettuata una scelta
004100160805     c                   if        codtel <> ' '
004200011126     c                   clear                   cmd
004300011120     c                   leave
004400011120     c                   end
004500011120     C                   ENDDO
004600011120     C                   SETON                                        LR
004700011120      **********************************************************************
004800011120     C     Initialize    BEGSR
004900011121     c*
005000160805     c                   if        codtel <> *blanks
005100160805     c     ' '           scan      codtel        len               2 0
005200011219     c                   sub       1             len
005300011219     c                   else
005400011219     c                   z-add     0             len
005500011219     c                   end
005600011120     C                   SETON                                        40
005700011120     C                   WRITE     CTL01
005800011120     C                   SETOFF                                       40
005900011120     C                   CLEAR                   RRN1
006000011218     c*Carico il subfile in ordine di codice autista
006100160805     C     codtel        SETLL     mzame05l
006200011120     C                   DO        *HIVAL
006300160805     C                   READ      mzame05l
006400160805     C                   IF        %eof(mzame05l)
006500011120     C                   LEAVE
006600011120     C                   END
006700160805     c                   if        codtel <> *blanks and
006800160805     c                             %subst(ametel: 1: len)>%subst(codtel: 1: len)
006900011218     c                   leave
007000011218     c                   end
007100160805     C                   MOVEL(P)  ametel        S01des
007200011218     C                   MOVEL     ametga        S01tga
007300011218     C                   MOVEL     ametgn        S01tgn
007400011120     C                   ADD       1             RRN1
007500011120     C                   WRITE     SFL01
007600011120     C                   ENDDO
007700160805     C                   CLEAR                   CODtel
007800011120     C                   EVAL      rcdnbr  = 1
007900011120     C                   ENDSR
008000011120     c**********************************************************************
008100011120     ** Controlli Sfl
008200011120     c**********************************************************************
008300011120     C     ctr           BEGSR
008400011120     C*
008500011121     c                   setoff                                       41
008600011120     c                   do        *hival        X                 4 0
008700011120     c     X             CHAIN     sfl01                              01
008800011120     C   01              LEAVE
008900011120     c                   if        s01opz <> ' '
009000160805     c                   movel     s01des        codtel
009100011120     c                   leave
009200011120     c                   end
009300011120     C* CONTROLLO SE EFFETTUATA RICERCA
009400011121     C                   IF        VDES <> ' ' AND VDES <> VDESS
009500011121     c                   seton                                        41
009600011219     C                   if        VDES <= S01DES
009700011121     c                   setoff                                       41
009800011120     C                   Z-ADD     X             rcdnbr
009900011121     C                   MOVEL     VDES          VDESS
010000011120     C                   LEAVE
010100011120     C                   END
010200011121     C                   END
010300011120     C                   ENDDO
010400011121     C  n41              clear                   VDES
010500011120     C*
010600011120     C                   ENDSR
010700011120     c**********************************************************************
010800011120     ** Operazioni iniziali.
010900011120     c**********************************************************************
011000011120     C     *INZSR        BEGSR
011100011120     C*
011200011120     C     *ENTRY        PLIST
011300160805     C                   PARM                    CODtel           20
011400011218     C                   PARM                    CMD               1
011500011218     c                   clear                   CMD
011600011121     c*
011700011120     C                   ENDSR
