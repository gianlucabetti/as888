000100950718     H*------------------------------------------------------------------------*
000200011219     H* RICERCA E SCELTA                                                       *
000300011219     H* PERMETTE LA RICERCA E LA SELEZIONE PER telaio                       O  *
000401011120     H*                                                                        *
000402011120     h* PARAMETRI:
000405011219     h* telaio         18  O
000406011219     h* numero rifer.   7,0O
000408011218     h* codice ritorno  1  O  (comando premuto)
000600950718     H*------------------------------------------------------------------------*
000700950718     H DECEDIT('0,') DATEDIT(*DMY.)
000800950718     F*------------------------------------------------------------------------*
000900950718     F* DATA BASE
001000950718     F*------------------------------------------------------------------------*
001100011219     FTRmz42D   CF   E             WORKSTN
001200011120     F                                     SFILE(SFL01:RRN1)
001800011219     Fmzame03l  IF   E           K DISK
018200011120     D WrkSort         S              1
018202011120     D VDESS           S                   LIKE(VDES)
018203011120     D rrn1            S              5i 0
020900011219     c* ordina il sfl per telaio
021200011120     C                   EXSR      Initialize
021201011121     c                   if        rrn1 = 0
021202011121     c                   seton                                        41
021203011121     c                   end
021600011120     c*
022000011120     C                   DO        *HIVAL
022007011121     c*
022008011121     C                   WRITE     TOP
022009011121     C                   WRITE     BOT
022200011120     C                   EXFMT     CTL01
022201011126     c                   if        *inkl
022202011126     c                   move      'L'           cmd
022300011126     C                   LEAVE
022301011126     c                   end
022302011121     c                   if        rrn1 = 0
022303011121     c                   iter
022304011121     c                   end
022709011121     c*Controlli
022710011120     c                   exsr      ctr
022711011121     c*effettuata una scelta
022712011219     c                   if        codtla <> ' '
022713011126     c                   clear                   cmd
022714011120     c                   leave
022715011120     c                   end
023100011120     C                   ENDDO
023300011120     C                   SETON                                        LR
023600011120      **********************************************************************
023900011120     C     Initialize    BEGSR
024000011121     c*
024001011219     c                   if        codtla <> *blanks
024002011219     c     ' '           scan      codtla        len               2 0
024003011219     c                   sub       1             len
024004011219     c                   else
024005011219     c                   z-add     0             len
024006011219     c                   end
024200011120     C                   SETON                                        40
024300011120     C                   WRITE     CTL01
024301011120     C                   SETOFF                                       40
024600011120     C                   CLEAR                   RRN1
025000011218     c*Carico il subfile in ordine di codice autista
025100011219     C     codtla        SETLL     mzame03l
025200011120     C                   DO        *HIVAL
025300011219     C                   READ      mzame03l
025600011219     C                   IF        %eof(mzame03l)
025601011120     C                   LEAVE
025602011120     C                   END
025604011219     c                   if        codtla <> *blanks and
025607011219     c                             %subst(ametla: 1: len)>%subst(codtla: 1: len)
025609011218     c                   leave
025610011218     c                   end
025800011219     C                   MOVEL(P)  ametla        S01des
025902011218     C                   MOVEL     ametga        S01tga
025903011218     C                   MOVEL     ametgn        S01tgn
026200011120     C                   ADD       1             RRN1
026300011120     C                   WRITE     SFL01
026600011120     C                   ENDDO
026601011219     C                   CLEAR                   CODtla
027001011120     C                   EVAL      rcdnbr  = 1
028000011120     C                   ENDSR
043101011120     c**********************************************************************
043300011120     ** Controlli Sfl
043301011120     c**********************************************************************
043400011120     C     ctr           BEGSR
043414011120     C*
043415011121     c                   setoff                                       41
043416011120     c                   do        *hival        X                 4 0
043417011120     c     X             CHAIN     sfl01                              01
043418011120     C   01              LEAVE
043419011120     c                   if        s01opz <> ' '
043420011219     c                   movel     s01des        codtla
043425011120     c                   leave
043426011120     c                   end
043427011120     C* CONTROLLO SE EFFETTUATA RICERCA
043431011121     C                   IF        VDES <> ' ' AND VDES <> VDESS
043432011121     c                   seton                                        41
043434011219     C                   if        VDES <= S01DES
043436011121     c                   setoff                                       41
043437011120     C                   Z-ADD     X             rcdnbr
043438011121     C                   MOVEL     VDES          VDESS
043440011120     C                   LEAVE
043441011120     C                   END
043442011121     C                   END
043444011120     C                   ENDDO
043445011121     C  n41              clear                   VDES
043446011120     C*
043447011120     C                   ENDSR
043448011120     c**********************************************************************
043449011120     ** Operazioni iniziali.
043450011120     c**********************************************************************
043451011120     C     *INZSR        BEGSR
043452011120     C*
043453011120     C     *ENTRY        PLIST
043455011219     C                   PARM                    CODtla           18
043459011218     C                   PARM                    CMD               1
043460011218     c                   clear                   CMD
043918011121     c*
044100011120     C                   ENDSR
