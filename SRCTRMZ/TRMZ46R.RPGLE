000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200031126     F* TRmz46R  *-----------------------------------------*
000300000000     F*----------------*                                   *
000400031126     F*          RICERCA EQUIPAGGIAMENTO                   *
000500000000     F*          --------------------                      *
000800000000     F*----------------------------------------------------*
000900031126     Ftabel00f  IF   E           K DISK
001000031126     FTRmz46d   CF   E             WORKSTN
001100000000     F                                     SFILE(GA02D4SF:NRR)
001101031126     D TIBS34DS      e ds                  inz
001102031126     D DDATIUTE      e ds                  inz
001103031126     D AZUTEDS       e ds                  inz extname(AZUTE00F)
001104031126     D Dsxu          e ds                  inz
001105031126     d dsxt          e ds                  inz
001106031126     D Ptr             S               *
001107031126     D                                     INZ(%ADDR(§xtcxu1))
001108031126     D equi            S                   LIKE(§xtcxu1)
001109031126     D                                     DIM(20)
001110031126     D                                     BASED(Ptr)
001111031126     Ddes              s             10    DIM(20)
001112031126     Dcomodo           s                   like(tblkey)
001200000000     C*----------------------------------------------------*
001300000000     C     *ENTRY        PLIST                                                  *
001600031126     C                   PARM                    tipo              1            *
001601031126     C                   PARM                    key               8            *
001602031126     C                   PARM                    des1             65            *
001603031126     C                   PARM                    des2             65            *
001604031126      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
001605031126     C     *dtaara       define    §azute        azuteds
001606031126     C     *dtaara       define    §datiute      ddatiute
001607031126      *
001608031126     C                   clear                   AzUteDs
001609031126     C                   clear                   DDatiUte
001610031126     C                   clear                   Tibs34Ds
001611031126     C                   in(E)     *dtaara
001612031126if  1C                   IF        %Error  or  RSUT = *blanks
001613031126     C                   call      'TIBS34R'
001614031126     C                   parm                    Tibs34Ds
001615031126     C                   in        *dtaara
001616031126e   1C                   ENDIF
001617031126w   1C                   if        DUTERR = 'E'
001618031126     c                   exsr      fine
001619031126     c                   end
001800000000     C*----------------------------------------------------*
002000031126     C     KTAb          KLIST                                                  *
002100031126     C                   KFLD                    codut             1 0          *
002101031126     C                   KFLD                    cod               2            *
002102031126     C     KTAb1         KLIST                                                  *
002103031126     C                   KFLD                    codut                          *
002104031126     C                   KFLD                    cod                            *
002105031126     C                   KFLD                    comodo                         *
002110031126     C     KTAb2         KLIST                                                  *
002111031126     C                   KFLD                    codut                          *
002112031126     C                   KFLD                    tblcod                         *
002113031126     C                   KFLD                    tblkey                         *
002114031126     c                   eval      cod = 'XT'
002115031126     c                   eval      codut = 1
002116031126     c                   eval      comodo = key
002200000000     C*----------------------------------------------------*
002300000000     C* PULIZIA SUBFILE
002400000000     C                   EXSR      PULIZ
002500000000     C* CARICAMENTO SUBFILE
002600000000     C                   EXSR      CARIC
002604031126     c                   if        tipo = 'R'
002700000000     C                   WRITE     GA02D401
002701031126     c                   end
002702031126     c                   do        *hival
002900000000     C     NRR           IFEQ      0
003000031126     C                   exsr      FINE
003100000000     C                   END
003200000000      *
003201031126     c                   if        tipo = 'R'
003300000000     C                   EXFMT     GA02D4SC
003400000000     C                   SETON                                        71
003500031126     C   Kc              exsr      FINE
003600000000     C                   READC     GA02D4SF                               21    *
003700000000     C   21              SETOFF                                       71
003800000000     C*
003900031126     C                   if        *in21 or s1sel = ' '
003901031126     c                   iter
003902031126     c                   end
003903031126     C                   END
004100000000     C*
004200031126     C                   MOVEl     S1cod         key
004201031126     C                   MOVEl     S1des1        des1
004202031126     C                   MOVEl     S1des2        des2
004203031126     c                   leave
004204031126     c                   enddo
004205031126     c                   exsr      fine
004600000000     C**----------------------
004700031126     C** fine
004800000000     C**----------------------
004900031126     C     fine          BEGSR
004901031126     c                   seton                                        lr
004902031126     c                   return
004903031126     c                   endsr
004904031126     C**----------------------
004905031126     C** PULIZIA DEL SUBFILE
004906031126     C**----------------------
004907031126     C     PULIZ         BEGSR
005000000000     C                   SETON                                        70
005100000000     C                   SETOFF                                       71
005200000000     C                   WRITE     GA02D4SC
005300000000     C                   SETOFF                                       70
005400000000     C                   SETON                                        71
005500000000     C                   ENDSR
005600000000     C**----------------------
005700000000     C** CARICAMENTO SUBFILE
005800000000     C**----------------------
005900000000     C     CARIC         BEGSR
006000000000     C                   Z-ADD     0             NRR               4 0
006001031126     c                   if        key = ' '
006100031126     C     KTAb          SETLL     tabel00f
006101031126     c                   else
006102031126     C     KTAb1         SETLL     tabel00f
006103031126     c                   end
006300000000     C                   DO        *HIVAL
006301031126     c                   if        key = ' '
006400031126     C     ktab          READe     tabel00f
006401031126     c                   else
006402031126     C     ktab1         READe     tabel00f
006403031126     c                   end
006404031126     c                   if        %eof(tabel00f)
006405031126     c                   leave
006406031126     c                   end
006501031126     c                   if        tblflg = '*'
006502031126     c                   iter
006503011221     c                   end
006600031126     C                   MOVEL     tblkey        S1cod
006601031126     C                   MOVEL     tbluni        dsxt
006602031126     C                   clear                   des
006603031126     c                   do        20            xx                2 0
006604031126     c                   if        equi(xx) <> ' '
006605031126     c                   exsr      srxu
006606031126     c                   end
006607031126     c                   enddo
006608031126     c                   eval      s1des1= des(1) + ' ' +
006609031126     c                             des(2) + ' ' +
006610031126     c                             des(3) + ' ' +
006611031126     c                             des(4) + ' ' +
006612031126     c                             des(5) + ' ' +
006613031126     c                             des(6)
006614031126     c                   eval      s1des2= des(7) + ' ' +
006615031126     c                             des(8) + ' ' +
006616031126     c                             des(9) + ' ' +
006617031126     c                             des(10)+ ' ' +
006618031126     c                             des(11)+ ' ' +
006619031126     c                             des(12)+ ' '
006620031126     C                   MOVEL     S1cod         comodo
006621031126     c     ktab1         setgt     tabel00f
006800000000     C                   ADD       1             NRR
006801031126     c                   if        tipo = 'R'
006900000000     C                   WRITE     GA02D4SF
006901031126     c                   end
007000031126     C                   ENDdo
007200000000     C                   ENDSR
007201031126      *---------------------------------------------------------------*
007202031126     C*  Eseguo posizionamento su XU
007203031126      *---------------------------------------------------------------*
007204031126     C     SRxu          BEGSR
007205031126     c                   clear                   dsxu
007207031126     c                   eval      tblcod = 'XU'
007208031126     c                   eval      tblkey = equi(xx)
007209031126     c     ktab2         chain     tabel00f
007210031126     c                   if        %found(tabel00f) and tblflg = ' '
007211031126     c                   movel     tbluni        dsxu
007214031126     C                   End
007215031126     c                   movel     §xudes        des(xx)
007219031126     C                   ENDSR
007300000000     C******************************************************
