000100950517     H DECEDIT('0,') DATEDIT(*DMY.)
000200110511     H* Trec01r *----------------------------------------------------*
000300110511     H* Tariffe "CE201"+Tariffe "CExxxx"=Tariffe "OUT"
000400950517     H*--------------------------------------------------------------*
000500110510     Ftrec01d   CF   E             WORKSTN
000501110510     d trec01ds      e ds                  inz
000502110510     D TIBS55ds      E DS
000503110510     d kpjba         e ds
000504110510     D CMD             S             48    DIM(1) CTDATA PERRCD(1)
000505110510     D WLBDA8          DS
000506110510     D  G08DAT                 1      8  0
000507110510     D  G08INV                 9     16  0
000508110510     D  G08ERR                17     17
000509110510     D  G08TGI                18     22  0
004600950517     C* Loop di gestione videata
004602050426     C     *ENTRY        PLIST
004603110510     C                   PARM                    kpjba
004604110510     C                   Z-ADD     48            LUNG             15 5
004605110510     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
004606110510     C* da/a cui spedire
004607110510     C                   CLEAR                   TIBS55ds
004608110510     C                   MOVEL     'L'           I50TLA
004609110510     C                   MOVEL     KNSIF         I50PSI
004610110510     C                   MOVEL     '001'         I50PPO
004611110510     C                   MOVEL     '046'         I50APO
004612110510     C                   CALL      'TIBS55R'
004613110510     C                   PARM                    TIBS55ds
004614050426     c*
004615110510     c* libreria 201
004616110510     c                   eval      lib201v = O50ALU
004617110510     c                   eval      ctrv = 'S'
004618110114     C                   SETON                                        LR
004619110228     c                   do        *hival
004701110510     C                   EXFMT     trec01
004702110510     c                   movea     '0000'        *in(01)
004703110510     C                   IF        *inkc = '1'
004704050426     c                   leave
004900950517     C                   END
004901110510     c* libreria output
004903110510     c                   if        liboutv <> *blanks
004904110510     C                   MOVEl(p)  CMD(1)        COMMAN           80
004905110510     c                   eval      %subst(comman:12:10) = liboutv
004906110510     C                   CALL      'QCMDEXC'                            01
004907110510     C                   PARM                    COMMAN
004908110510     C                   PARM                    LUNG
004909110510     c                   else
004910110510     c                   seton                                        01
004911110510     c                   end
004912110510     c   01              iter
004913110510     c* libreria input
004914110510     c                   if        libinpv <> *blanks
004915110510     C                   MOVEl(p)  CMD(1)        COMMAN
004916110510     c                   eval      %subst(comman:12:10) = libinpv
004917110510     C                   CALL      'QCMDEXC'                            02
004918110510     C                   PARM                    COMMAN
004919110510     C                   PARM                    LUNG
004920110510     c   02              iter
004921110510     c                   else
004922110510     c                   eval      libinpv = liboutv
004923110510     c                   end
004924110510     c* data decorrenza
004925110510     C                   clear                   g08inv
004926110510     C                   eval      G08dat  = dtadecv
004927110510     C                   MOVEL     '0'           G08ERR
004928110510     C                   CALL      'XSRDA8'
004929110510     C                   PARM                    WLBDA8
004930110510     C                   if        G08err ='1'
004931110510     c                   seton                                        04
004932110510     c                   iter
004933110510     c                   end
004934110510     c                   move      g08dat        dtadecv
004942110510     c*
004943110510     c                   if        *inkf
004944110510     c                   eval      d01libout = liboutv
004945110510     c                   eval      d01libinp = libinpv
004946110510     c                   eval      d01lib201 = lib201v
004947110510     c                   eval      d01flgctr = ctrv
004948110510     c                   movel(p)  trec01ds      kpjbu
004949110510     c                   call      'TRECIMP0C'
004950110510     c                   parm                    kpjba
004951050426     c                   leave
004952050426     c                   end
005000050426     C                   ENDdo
005001110510**
005100110510CHKOBJ OBJ(          ) OBJTYPE(*lib)
