000100950517     H DECEDIT('0,') DATEDIT(*DMY.)
000200110511     H* Trec00r *----------------------------------------------------*
000300110510     H* calcola importo di una voce
000400950517     H*--------------------------------------------------------------*
000500110510     Ftrec00d   CF   E             WORKSTN
000502110510     Fecevt01l  iF   E           k disk
000503110510     d trec00ds      e ds                  inz
000504110510     d kpjba         e ds
000505110510     D WLBDA8          DS
000506110510     D  G08DAT                 1      8  0
000507110510     D  G08INV                 9     16  0
000508110510     D  G08ERR                17     17
000509110510     D  G08TGI                18     22  0
000510110510     D datads          DS
000511110510     D  annov                  1      4  0
000512110510     D  mesev                  5      6  0
000513110510     D  giornov                7      8  0
000514110510     D  datav                  1      8  0
004600950517     C* Loop di gestione videata
004602050426     C     *ENTRY        PLIST
004603110510     C                   PARM                    kpjba
004607050426     c*
004608110114     C                   SETON                                        LR
004609110510     c                   clear                   datav
004610110510     c                   move      *year         annov
004614110228     c                   do        *hival
004701110510     C                   EXFMT     trec00
004702110510     c                   setoff                                       010203
004703110510     C                   IF        *inkc = '1'
004704050426     c                   leave
004900950517     C                   END
004901110510     c* anno
004902110510     c                   if        annov = 0
004903110510     c                   seton                                        02
004904110510     c                   iter
004905110510     c                   end
004906110510     c* mese
004907110510     c                   if        mesev = 0  and giornov <> 0
004908110510     c                   seton                                        03
004909110510     c                   iter
004910110510     c                   end
004911110510     c* voce
004912110510     c                   clear                   desvocev
004913110510     c                   if        vocev <> 0
004914110510     c     vocev         setgt     ecevt01l
004915110510     c     vocev         readpe    ecevt01l
004916110510     c                   if        %eof(ecevt01l)
004917110510     c                   seton                                        01
004918110510     c                   iter
004919110510     c                   else
004920110510     c                   movel     EVTVOD        desvocev
004921110510     c                   if        evttvo <> 'V'
004922110510     c                   seton                                        01
004923110510     c                   iter
004924110510     c                   end
004925110510     c                   end
004926110510     c                   else
004927110510     c                   seton                                        01
004928110510     c                   iter
004929050426     c                   end
004930110510     c* periodo
004931110510     c                   if        mesev <> 0
004932110510     C                   eval      com08 = datav
004933110510     c                   if        giornov = 0
004934110510     c                   move      01            com08             8 0
004935110510     c                   end
004936110510     C                   eval      G08inv  = com08
004937110510     C                   MOVEL     '3'           G08ERR
004938110510     C                   CALL      'XSRDA8'
004939110510     C                   PARM                    WLBDA8
004940110510     C                   if        G08err ='1'
004941110510     c                   seton                                        03
004942110510     c                   iter
004943110510     c                   end
004944110510     c                   end
004945110510     C                   IF        *inkf = '1'
004946110510     c                   move      vocev         d00voce
004947110510     C                   eval      d00dta = datav
004948110510     c                   movel(p)  trec00ds      kpjbu
004949110510     c                   call      'TRECVOC1C'
004950110510     c                   parm                    kpjba
004951110511     c                   leave
004952110510     C                   END
005000050426     C                   ENDdo
