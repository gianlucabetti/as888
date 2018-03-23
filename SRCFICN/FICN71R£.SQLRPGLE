000100940912     H DECEDIT('0,') DATEDIT(*DMY.)
000200130515      * Ficn71r *----------------------------------------------------*
006800900517      *--------------------------------------------------------------*
006900130515     FFicn71d   CF   E             WORKSTN
007300130412     F                                     SFILE(lst4S08:NRR8)
007301130515     Fazorg01l  if   E           k disk
007302130515     Ffiapd01l  if   E           k disk
007303130515     d kpjba         e ds
007304130515     d ficn71ds      e ds
016600130412     D* numeri piombi PER CONTROLLI DI RECORD DOPPIO
016700130417     D PIO2            S              8    DIM(500)
016900020805      *
016901130515     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
029300110512     d Indx            s              3  0
029400130415     d parx            s              3  0
029500130510     d XI              s              3  0
029600110530     d EmisD06         s              1
029601130515     d vidpmb          s            100
704000130412     C**************************************************************************
704100130412     C*  GESTIONE VIDEATA INSERIMENTO piombi
704200130412     C**************************************************************************
704301130515     c     *entry        plist
704302130515     c                   parm                    kpjba
704303130515     c                   movel     kpjbu         ficn71ds
704304130515      *
704305130515     c     kpdr          klist
704306130515     c                   kfld                    tipo              1
704307130515     c                   kfld                    vpdr
704308130515     c                   move      'A'           tipo              1
704309130515      *
704310130515     c                   eval      vidtes = '    Inserimento Piombi '
704311130515     c                   eval      vidpgm = 'FICN71R'
704312130515     c                   eval      vTER   =  I71TER
704313130515     c                   eval      vFGP   =  I71FGP
704314130515     c                   eval      vFGA   =  I71FGA
704315130515     c                   eval      vVAD   =  I71VAD
704316130515     c                   eval      vORP   =  I71ORP
704317130515     c                   eval      vORA   =  I71ORA
704318130515     c                   eval      vDDC   =  I71DDP
704319130515     c                   eval      vPDR   =  I71PDR
704320130515     c                   movel     IO71PIO       vidpmb
704321130515     c     vter          chain     azorg01l
704322130515     c                   movel     orgdes        vterd
704323130515     c     vfgp          chain     azorg01l
704324130515     c                   movel     orgdes        vfgpd
704325130515     c     vfga          chain     azorg01l
704326130515     c                   movel     orgdes        vfgad
704327130515     c     kpdr          chain     fiapd01l
704328130515     c                   movel     apdrsc        vpdrd
704329130515
704400130415     c* Se il campo piombi è pieno imposto una riga per ogni costante
704500130415     c* spaziata
704600130415     c                   seton                                        35
704700130415     C                   WRITE     LSt4C08
704800130415     c                   setoff                                       3578
704900130415     c
705000130415     C                   Z-ADD     0             NRR8              4 0
705100130430    1c                   if        vidpmb<>*blanks
705200130415     c                   eval      Indx=1
705300130415     c                   eval      Parx=1
705400130515    2c                   dow       Indx>0 and parx<=48
705500130415     c                   eval      Indx=%scan(' ':vidpmb:Parx)
705600130415     C                   clear                   V8CNOT
705700130430    3c                   if        indx>0 and Indx=parx
705800130430     c                   eval      parx=parx+1
705900130430   x3c                   else
706000130430    4c                   if        Indx>0
706100130430     c*****              leave
706200130415     c
706300130415     c                   eval      v8cnot=%subst(vidpmb:parx:(indx-parx))
706400130415     c                   eval      parx=indx+1
706500130415     c                   else
706600130515     c                   eval      indx=48-parx+1
706700130415     c                   eval      v8cnot=%subst(vidpmb:parx:Indx)
706800130415     c                   clear                   indx
706900130430    4c                   endif
707000130415     c
707100130415     C                   ADD       1             NRr8
707200130415     C                   CLEAR                   V8CSCE
707300130415     C                   WRITE     LSt4S08
707400130415     C                   SETON                                        78
707500130430    3c                   endif
707600130430    2c                   enddo
707700130430    1c                   endif
707800130415     C                   Z-ADD     1             V8CNBR
707900130412     c
708000130412     c                   write     lst4T01
708200130412     c
708300130412     C     EMD09         TAG
708400130412     C                   WRITE     LSt4C08
708500130412     C                   EXFMT     LSt4D09
708600130502     C                   SETOFF                                       28
708700130412     c
708800130412     C* SCRIVO SUBFILE (SE CHI SONO GIA' IMMESSO NON LO RICARICO E NON
708900130412     C* DO ERRORE)
709000130415    1C     V9CNOT        IFNE      *BLANKS
709100130412     C     V9CNOT        LOOKUP    PIO2                                   31
709200130412     c
709300130415    2C     *IN31         IFEQ      *OFF
709400130430     c* Se non è tutto numerico --> errore
709500130430     c                   testn                   v9cnot               33
709600130430     c                   move      v9cnot        alfa1             1
709700130430     c* Errore se non numerico
709800130430     c                   if        not *in33 or alfa1<'0'
709900130515     c                   eval      vidmsg=msg(01)
710000130430     c                   seton                                        28
710100130430     c                   goto      emd09
710200130430     c                   endif
710300130514     c* Errore se è il sesto
710400130514     c                   if        nrr8 >=  5
710500130515     c                   eval      vidmsg=msg(02)
710600130514     c                   seton                                        28
710700130514     c                   goto      emd09
710800130514     c                   endif
710900130430     c
711000130412     C                   ADD       1             NRr8
711100130412     C                   Z-ADD     NRR8          V8CNBR
711200130412     C                   MOVEL     V9CNOT        V8CNOT
711300130412     C                   CLEAR                   V8CSCE
711400130412     C                   WRITE     LSt4S08
711500130412     C                   SETON                                        78
711600130412     C                   ADD       1             I2                3 0
711700130412     C                   MOVE      V9CNOT        PIO2(I2)
711800130415    2C                   ENDIF
711900130412     C                   CLEAR                   V9CNOT
712000130415    1C                   ENDIF
712100130412     c
712200130412     C* PER F6 ESCO DALLA VIDEATA DI IMMISSIONE "CHI SONO"
712300130415    2C     *INKF         IFEQ      *OFF
712400130412     C                   GOTO      EMD09
712500130415    2C                   ENDIF
712600130412     c
712700130412     c* Quando esco scrivo il campo piombi
712800130415    2c                   if        nrr8>0
712900130412     c                   z-add     1             nrr8
713000130415     c                   clear                   vidpmb
713100130412     c     nrr8          chain     lst4s08
713200130412     c
713300130412     c                   dow       %found
713400130415     c                   if        v8cnot<>*blanks and v8csce<>'A'
713500130412     c
713600130412     c                   if        vidpmb<>*blanks
713700130412     c                   eval      vidpmb=%trim(vidpmb) +' '+v8cnot
713800130412     c                   else
713900130412     c                   eval      vidpmb=v8cnot
714000130412     c                   endif
714100130412     c                   endif
714200130412     c
714300130412     c                   add       1             nrr8
714400130412     c     nrr8          chain     lst4s08
714500130412     c                   enddo
714600130412     c                   endif
714700130412     c
714800130515     C                   seton                                        lr
714801130515      *__________________________________________________________________________
714802130515** SCHIERA MSG - MESSAGGI DI ERRORE
714900130515Il codice del piombo deve essere formato da 8 numeri                          01
715000130515Non è possibile inserire con lettura barcode più di 5 piombi                  02
