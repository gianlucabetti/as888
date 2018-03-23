000100080428     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200080424      * Fior47R *----------------------------------------------------*
000300080424      *         - Interrogazione/ricerca DISTINTE chiuse su PDA      *
000500080424      *--------------------------------------------------------------*
002600080430     Ffior47D   CF   E             WORKSTN
002800080424     F                                     SFILE(or47SF1:nrr)
002900080424     Ffirpt02l  if   e           k disk
002901080430     Ffiapd01l  if   e           k disk
002902080430     Fazorg01L  if   e           k disk
003001080428      *------------------------------------------------------------------------*
003002080428      *  RIEPILOGO INDICATORI
003003080428      *------------------------------------------------------------------------*
003011080428      *  20 - gestione subfile
003012080428      *  21 - gestione subfile
003013080428      *  22 - sflnxtchg
003014080428      *  23 - gestione subfile
003015080428      *  28 - errore generico
003016080428      *  30 - generico operazioni i/o
003021080428
003022080428      *------------------------------------------------------------------------*
003023080428      *   S C H I E R E
003024080428      *------------------------------------------------------------------------*
003025080430
003026080428      *------------------------------------------------------------------------*
003027080428      *   D S   I N T E R N E / E S T E R N E
003028080428      *------------------------------------------------------------------------*
003029080428     d ddatiute      e ds
003030080428     d azuteds       e ds                  extname(AZUTE00F)
003031080428     d tibs34ds      e dS                  inz
003032080428     d KPJBA         e dS
003033080430     d Fior47ds      e dS
003034080430     d og150         e ds                  inz
003035080430
003036080430     d wlbdat          ds
003037080430     d  g02dat                 1      8  0
003038080430     d  g02inv                 9     16  0
003039080430     d  g02err                17     17
003040080430     d  g02tgi                18     22  0
003041080430
003042080428      *------------------------------------------------------------------------*
003043080428      *   V A R I A B I L I
003044080428      *------------------------------------------------------------------------*
003045080430     d ktipa           s                   like(apdtip) inz('A')
003046080430     d nrr             s              4  0
003047080430
003048080428      *---------------------------------------------------------------*
003049080428      *
    Ciclo Principale           ?
003050080428      *---------------------------------------------------------------*
003055080428      * gestione della videata di intrrrogazione ricerca distinta
003056080430     c                   exsr      sr_wrtsbf
003057080430      * verifico se scritto almenop un record
003058080430     c                   If        nrr = *zeros
003059080430      * emetto la videata di mancanza di record da selelzionare
003060080505     c                   seton                                        21
003061080430     c                   setoff                                       20
003062080430     c                   write     or47z02
003064080430     c                   write     or47v02
003065080430     c                   exfmt     or47ct1
003066080430     c                   if        *inkc
003067080430     c                   goto      fine
003068080430     c                   endif
003069080430     c                   endif
003070080430
003071080430      * gestione del subfile pieno
003072080505     c                   seton                                        212023
003073080430     c                   dow       *inkc = *off
003074080430     c                   write     or47z02
003075080430     c                   exfmt     or47ct1
003076080430     c                   dow       not *inkc
003077080430     c                   readc     or47sf1
003078080430     c                   if        not %eof
003079080430     c                             and s1sce = '1'
003080080430      * ritorno i dati alla ds
003081080430     c                   eval      or47nfvo = s1cnfv
003082080430     c                   goto      fine
003083080430     c                   else
003084080430     c                   leave
003085080430     c                   endif
003086080430     c                   enddo
003087080430     c                   enddo
003088080430
003089080430     c     fine          tag
003090080430     c                   movel(p)  Fior47ds      kpjbu
003091080430     c                   seton                                        lr
003154080428
003155080428      *---------------------------------------------------------------*
003156080430      * ? Scrittura subfile
003157080428      *---------------------------------------------------------------*
003158080430     c     Sr_wrtsbf     BEGSR
003159080428
003161080430      * pulizia subfile
003162080430     c                   setoff                                       2021
003163080430     c                   write     or47ct1
003164080430     c                   seton                                        21
003165080430     c                   z-add     0             nrr
003166080430     c                   z-add     1             c1nrr
003167080430
003168080430     c     krpt0202      setll     firpt02l
003170080430     c                   do        *hival
003171080430
003172080430     c     krpt0202      reade     firpt02l
003173080430      * fine file
003174080430     c                   if        %eof(firpt02l)
003175080430     c                   leave
003176080430     c                   endif
003177080430
003179080430      * verifico se  distinta mattina o pomeriggio o messaggerie o parcel in base alle selezioni
003180080430      * del video chiamante
003183080430      *
003184080430      * Tipo Uscita Mattina/Pomeriggio
003185080430     c                   if        or47fppi <> ' ' and
003186080430     c                             ((or47fppi='M' and rptfpp<>'M') or
003187080430     c                              (or47fppi='P' and rptfpp<>'P'))
003188080430     c                   iter
003189080430     c                   endif
003191080430      * Parcel / messaggerie / tutte
003192080430     c                   if        or47tpmi <> ' ' and *in08 and
003194080430     c                             ((or47tpmi='M' and rpttpm<>'M') or
003195080430     c                              (or47tpmi='P' and rpttpm ='M'))
003196080430     c                   iter
003197080430     c                   endif
003198080430      * se record ok scrivo il subfile
003199080430
003200080430      * scrivo il subfile
003201080430     c                   add       1             nrr
003202080430     c                   eval      s1cnfv = rptnfv
003203080430      * Giro data Distinta
003204080430     c                   move      rptdfv        g02inv
003205080430     c                   movel     '3'           g02err
003206080430     c                   call      'XSRDA8'
003207080430     c                   parm                    wlbdat
003208080430      *
003209080430     c                   move      g02dat        s1cdfv
003210080430      * padroncino
003211080430     c                   eval      s1caut = rptpdr
003212080430     c     kapd          chain     fiapd01l
003213080430     c                   if        %found(fiapd01l)
003214080430     c                   eval      s1DAUT = apdrsc
003215080430     c                   else
003216080430     c                   clear                   s1DAUT
003217080430     c                   end
003218080430      * Tipo uscita
003219080430     c                   if        rptfpp  ='M'
003220080430     c                   movel     'Mat'         s1Tusc
003221080430     c                   else
003222080430     c                   move      'Pom'         s1Tusc
003223080430     c                   end
003228080430      *
003229080430      * Tipo distinta parcel/messaggerie
003230080430     c                   if        *in08
003231080430     c                   if        rpttpm='M'
003232080430     c                   movel     'Mes'         s1CTPM
003233080430     c                   endif
003234080430     c                   if        rpttpm<>'M'
003235080430     c                   movel     'Prc'         s1CTPM
003236080430     c                   endif
003237080430     c                   endif
003238080430
003239080430     c                   write     or47sf1
003242080430
003243080430     c                   enddo
003244080430
003245080430     c                   endsr
003246080428      *---------------------------------------------------------------*
003247080428      *
    OPERAZIONI INIZIALI        ?
003300080428      *---------------------------------------------------------------*
003400080428     c     *INZSR        BEGSR
003500080428
003600080428      * Reperimento parametri
003700080428     c     *Entry        Plist
003800080428     c                   Parm                    Kpjba
004000080428      * Kpjbu duiversa da blank richiamato
004001080428     c                   If        Kpjbu <> *blanks
004100080428     c                   movel     kpjbu         Fior47ds
004102080428     c                   endif
005500080428      *
005501080428
005502080428     c     *dtaara       define    §azute        azuteds
005503080428     c     *dtaara       define    §datiute      ddatiute
005504080428     c                   in(E)     *dtaara
005505080428     c                   IF        %Error  or  RSUT = *blanks
005506080428     c                   call      'TIBS34R'
005507080428     c                   parm                    Tibs34Ds
005508080428     c                   in        *dtaara
005509080428     c                   Endif
016500080428      ***
016600080428      * GIRO DATA DEL GIORNO: LA PRENDO DA TIME
016700080428      ***
016800080428     c                   Time                    w0140            14 0
016900080428     c                   Move      w0140         udate8            8 0
017000080428     c                   Movel     w0140         hhmm              4 0          *ORA/MINUTI
017100080428      *
017200080428     c                   z-add     udate8        g02dat                         *GG/MM/AAAA
017300080428     c                   Movel     *blank        g02err
017400080428     c                   Call      'XSRDA8'
017500080428     c                   Parm                    wlbdat
017600080428     c                   z-add     g02inv        dateu8            8 0          *AAAA/MM/GG
017601080430      * recupero se filiale di gestione è abilitato a parcel/messaggerie
017602080430     c                   eval      c1cfgs = or47fgsi
017603080430     c     or47fgsi      chain     azorg01l
017604080430    1c                   if        %found(azorg01l)
017605080430     c                   movel     orgdf0        og150
017606080430      *
017607080430     c* p.o. con piking messaggerie
017608080430     c                   if        §ogpkm='S'
017609080430     c                   seton                                        08
017610080430     c                   endif
017611080430     c                   endif
017705080428      *
017706080428     c     Krpt0202      klist
017707080428     c                   kfld                    or47fgsi
017708080428     c                   kfld                    or47dfvi
017709080430
017711080430     c     Kapd          klist
017712080430     c                   kfld                    ktipa
017713080430     c                   kfld                    rptpdr
017800080428     c                   Endsr
