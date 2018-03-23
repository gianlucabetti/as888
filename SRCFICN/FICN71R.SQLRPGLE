000100940912     H DECEDIT('0,') DATEDIT(*DMY.)
000200130515      * Ficn71r *----------------------------------------------------*
000300900517      *--------------------------------------------------------------*
000400130515     FFicn71d   CF   E             WORKSTN
000500130412     F                                     SFILE(lst4S08:NRR8)
000600130515     Fazorg01l  if   E           k disk
000700130515     Ffiapd01l  if   E           k disk
000800130515     d kpjba         e ds
000900130515     d ficn71ds      e ds
001000130520     d dtaiso          s               d
001100130520     d dtaeur          s               d   datfmt(*eur)
001200130520     D* numeri piombi PER CONTROLLI DI RECORD DOPPIO
001300130417     D PIO2            S              8    DIM(500)
001400020805      *
001500130515     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
001600110512     d Indx            s              3  0
001700130415     d parx            s              3  0
001800130510     d XI              s              3  0
001801130524     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001802130524     D CNCR80        E DS
001900110530     d EmisD06         s              1
002000130522     d vidpmb          s             50
002100130412     C**************************************************************************
002200130412     C*  GESTIONE VIDEATA INSERIMENTO piombi
002300130412     C**************************************************************************
002400130415     c                   seton                                        35
002500130415     C                   WRITE     LSt4C08
002600130415     c                   setoff                                       3578
002700130415     C                   Z-ADD     0             NRR8              4 0
002800130520     c* Se il campo piombi è pieno imposto una riga per ogni costante
002900130520     c* spaziata-> SFL
003000130430    1c                   if        vidpmb<>*blanks
003100130520     c                   exsr      carsfl
003200130520     c                   end
003300130520     c* testata
003400130520     C                   Z-ADD     1             V8CNBR
003500130412     c                   write     lst4T01
003600130520     c                   do        *hival
003700130412     C                   WRITE     LSt4C08
003800130412     C                   EXFMT     LSt4D09
003900130502     C                   SETOFF                                       28
004000130520     C* PER F6 ESCO
004100130520    2C                   iF        *inkf
004200130520     C                   leave
004300130520    2C                   ENDIF
004400130412     C* SCRIVO SUBFILE (SE CHI SONO GIA' IMMESSO NON LO RICARICO E NON
004500130412     C* DO ERRORE)
004600130415    1C     V9CNOT        IFNE      *BLANKS
004700130412     C     V9CNOT        LOOKUP    PIO2                                   31
004800130415    2C     *IN31         IFEQ      *OFF
004900130430     c* Se non è tutto numerico --> errore
005000130430     c                   testn                   v9cnot               33
005100130430     c                   move      v9cnot        alfa1             1
005200130430     c* Errore se non numerico
005300130430     c                   if        not *in33 or alfa1<'0'
005400130515     c                   eval      vidmsg=msg(01)
005500130430     c                   seton                                        28
005600130520     c                   iter
005700130430     c                   endif
005800130514     c* Errore se è il sesto
005900130514     c                   if        nrr8 >=  5
006000130515     c                   eval      vidmsg=msg(02)
006100130514     c                   seton                                        28
006200130520     c                   iter
006300130514     c                   endif
006400130522     c* se non ci sono errori carico il piombo nel sfl
006500130412     C                   ADD       1             NRr8
006600130412     C                   Z-ADD     NRR8          V8CNBR
006700130522     C                   MOVEL(p)  V9CNOT        V8CNOT
006800130412     C                   CLEAR                   V8CSCE
006900130412     C                   WRITE     LSt4S08
007000130412     C                   SETON                                        78
007100130412     C                   ADD       1             I2                3 0
007200130412     C                   MOVE      V9CNOT        PIO2(I2)
007300130415    2C                   ENDIF
007400130412     C                   CLEAR                   V9CNOT
007500130415    1C                   ENDIF
007600130520     c                   enddo
007700130412     c* Quando esco scrivo il campo piombi
007800130415    2c                   if        nrr8>0
007900130412     c                   z-add     1             nrr8
008000130415     c                   clear                   vidpmb
008100130412     c     nrr8          chain     lst4s08
008200130412     c                   dow       %found
008300130415     c                   if        v8cnot<>*blanks and v8csce<>'A'
008400130412     c                   if        vidpmb<>*blanks
008500130412     c                   eval      vidpmb=%trim(vidpmb) +' '+v8cnot
008600130412     c                   else
008700130412     c                   eval      vidpmb=v8cnot
008800130412     c                   endif
008900130412     c                   endif
009000130412     c                   add       1             nrr8
009100130412     c     nrr8          chain     lst4s08
009200130412     c                   enddo
009300130412     c                   endif
009400130520     c*
009500130522     c                   movel     vidpmb        iO71Pmb
009600130520     c                   movel(p)  ficn71ds      kpjbu
009700130520     c*
009800130515     C                   seton                                        lr
009900130520     C**************************************************************************
010000130520     C*  carico sfl piombi
010100130520     C**************************************************************************
010200130520     c     carsfl        begsr
010300130520     c                   eval      Indx=1
010400130520     c                   eval      Parx=1
010500130522    2c                   dow       Indx>0 and parx<=50
010600130520     c                   eval      Indx=%scan(' ':vidpmb:Parx)
010700130520     C                   clear                   V8CNOT
010800130520    3c                   if        indx>0 and Indx=parx
010900130520     c                   eval      parx=parx+1
011000130520   x3c                   else
011100130520    4c                   if        Indx>0
011200130520     c                   eval      v8cnot=%subst(vidpmb:parx:(indx-parx))
011300130520     c* carico schiera piombi
011400130520     c                   if        (indx-parx) <= 8
011500130520     c                   movel     v8cnot        com08             8
011600130520     C     com08         LOOKUP    PIO2                                   31
011700130520     c                   if        not *in31
011800130520     C                   ADD       1             I2                3 0
011900130520     C                   MOVE      com08         PIO2(I2)
012000130520     c                   end
012100130520     c                   end
012200130520     c                   eval      parx=indx+1
012300130520     c                   else
012400130522     c                   eval      indx=50-parx+1
012500130520     c                   eval      v8cnot=%subst(vidpmb:parx:Indx)
012600130520     c                   clear                   indx
012700130520    4c                   endif
012800130520     c*
012900130520     C                   ADD       1             NRr8
013000130520     C                   CLEAR                   V8CSCE
013100130520     C                   WRITE     LSt4S08
013200130520     C                   SETON                                        78
013300130520    3c                   endif
013400130520    2c                   enddo
013500130520     c                   endsr
013600130520     C**************************************************************************
013700130520     C*  subroutine inizio
013800130520     C**************************************************************************
013900130520     c     *inzsr        begsr
014000130520     c     *entry        plist
014100130520     c                   parm                    kpjba
014200130520     c                   movel     kpjbu         ficn71ds
014201130524     C                   Z-ADD     1             CODUT
014202130524     C                   CALL      'X§PARUT'
014203130524     C                   PARM                    UTEDSE
014204130524     C                   MOVEL     REC80         CNCR80
014205130524     C                   MOVEL     Ragut         vidrsu
014300130520      *
014400130520     c     kpdr          klist
014500130520     c                   kfld                    tipo              1
014600130520     c                   kfld                    vpdr
014700130522     c                   move      'D'           tipo              1
014800130520      *
014900130520     c                   eval      vidtes = '    Inserimento Piombi '
015000130520     c                   eval      vidpgm = 'FICN71R'
015100130520     c                   eval      vTER   =  I71TER
015200130520     c                   eval      vFGP   =  I71FGP
015300130520     c                   eval      vFGA   =  I71FGA
015400130520     c                   select
015500130520     c                   when      I71VAD = 'A'
015600130520     c                   eval      vVAD = 'AFFLUENZA'
015700130520     c                   when      i71vad='D'
015800130520     c                   eval      vVAD = 'DEFLUENZA'
015900130520     c                   other
016000130520     c                   clear                   vvad
016100130520     c                   end
016200130520     c                   eval      vORP   =  I71ORP
016300130520     c                   eval      vORA   =  I71ORA
016400130522     c                   movel     I71DDc        dtaiso
016500130520     c                   movel     dtaiso        dtaeur
016600130520     c                   movel     dtaeur        vddc
016700130520     c                   eval      vPDR   =  I71PDR
016800130522     c                   movel     iO71Pmb       vidpmb
016900130520     c     vter          chain     azorg01l
017000130520     c                   if        not %found(azorg01l)
017100130520     c                   seton                                        lr
017200130520     c                   return
017300130520     c                   end
017400130520     c                   movel     orgdes        vterd
017500130520     c     vfgp          chain     azorg01l
017600130520     c                   if        not %found(azorg01l)
017700130520     c                   seton                                        lr
017800130520     c                   return
017900130520     c                   end
018000130520     c                   movel     orgdes        vfgpd
018100130520     c     vfga          chain     azorg01l
018200130520     c                   if        not %found(azorg01l)
018300130520     c                   seton                                        lr
018400130520     c                   return
018500130520     c                   end
018600130520     c                   movel     orgdes        vfgad
018700130520     c                   if        i71pdr <> 0
018800130520     c     kpdr          chain     fiapd01l
018900130520     c                   if        not %found(fiapd01l)
019000130520     c                   seton                                        lr
019100130520     c                   return
019200130520     c                   end
019300130522     c                   movel     apdrsf        vpdrd
019400130520     c                   else
019500130520     c                   clear                   vpdrd
019600130520     c                   end
019700130520     c                   endsr
019800130515      *__________________________________________________________________________
019900130515** SCHIERA MSG - MESSAGGI DI ERRORE
020000130515Il codice del piombo deve essere formato da 8 numeri                          01
020100130515Non è possibile inserire con lettura barcode più di 5 piombi                  02
