000100050519      *---------------------------------------------------------------*
000200050519      *?STAMPA LETTERA   APERTURA EVENTO                             ?*
000300050519      *---------------------------------------------------------------*
000400050519
000500050519     h decedit('0,') datedit(*dmy/)
000600050519
000700050519      *---------------------------------------------------------------*
000800050519
000900050519     fFNDET01L  if   e           k disk
001000050519     fFNDCS01L  if   e           k disk
001100050519     fAZORG01L  if   e           k disk
001200050530     fTNTBE01L  if   e           k disk
001300050519      *
001400050519     fFIDN06P1  o    e             printer usropn
001500050519     f                                     oflind(*in01)
001600050519     ***f                                     infds(INFds_P1)
001700050519
001800050519      *---------------------------------------------------------------*
001900050520
002000050520     d C_Cmd_1         c                   const('DLTOVR  FILE(FIDN06P1) +
002100050520     d                                            LVL(*JOB)')
002200050519
002300050519      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002400050519
002500050519     d KPJBA         e ds
002600050519     d FIDN06ds      e ds
002700050519      *
002800050519      * - Parametri x Controllo profilo utenti
002900050519     d TIBS34ds      e ds                  inz
003000050519      * - Ds di riferimento al file esterno AZUTE00F
003100050519     d AZUTEds       e ds                  extname(AZUTE00F)
003200050519      * - Ds per dati organigramma
003300050519     d dDatiUte      e ds
003400050519      *
003500050519      * Parametri x invio e-mail
003600050530     d dmradan       e ds
003700050530     d trtcm1ds      e ds
003800050519      *
003900050519     d TIBS02ds      e ds                  inz
004000050519     d  T02mod       e                     inz('C')
004100050519     d dSTD4         e ds                  inz
004200050520     d  §STD4flg     e                     inz('L')
004300050519     d dTAD          e ds                  inz
004400050519      *
004500050519     d FNDETds       e ds                  inz  extname(FNDET00F)
004600050519      *
004700050519     d DS_KEV          ds                  inz
004800050519     d  wKEVaae                       4a   inz(*zeros)
004900050519     d  wKEVnev                       7a   inz(*zeros)
005000050519      *
005100050519     d WLBdat          ds                  inz
005200050519     d  G02dat                 1      8  0 inz
005300050519     d  G02inv                 9     16  0 inz
005400050519     d  G02err                17     17    inz('3')
005500050519     d  G02tgi                18     22  0 inz
005600050519     *** *
005700050519     ***d INFds_P1        ds
005800050519     ***d  Cur_LineP1           367    368I 0
005900050519      *
006000050519     d Status         sds
006100050519     d  SDSpgm           *proc
006200050519
006300050519      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
006400050519
006500050519     d Qcmd            s            500    inz
006600050519     d Qlen            s             15  5 inz(500)
006700050519
006800050530     D CMM             S             70    DIM(1) CTDATA PERRCD(1)
006900050530
007000050519      *---------------------------------------------------------------*
007100050519
007200050519      * - FNDET01L
007300050519     c     K02DET01      klist
007400050519     c                   kfld                    I06aae
007500050519     c                   kfld                    I06nev
007600050519      * - FNDCS01L
007700050519     c     K07DCS01      klist
007800050519     c                   kfld                    DCStpd
007900050519     c                   kfld                    DCSnkt
008000050519     c                   kfld                    DCSstd
008100050519     c                   kfld                    DCSdim
008200050519     c                   kfld                    DCShim
008300050519     c                   kfld                    DCSnks
008400050519     c                   kfld                    DCStrc
008500050530      * - TNTBE01L
008600050530     C     ktbe          klist
008700050530     C                   kfld                    tbecod
008800050530     C                   kfld                    tbeke1
008900050530     C                   kfld                    tbeke2
009000050530     c
009100050519
009200050519      *---------------------------------------------------------------*
009300050519      *  RIEPILOGO INDICATORI                                         *
009400050519      *---------------------------------------------------------------*
009500050519      * 01    - OverFlow                                              *
009600050520      * 02    - Toglie "spaziature" a sx per e-mail                   *
009700050519      * 90    - Chiamata a QCMDEXC (ovrprtf) in errore                *
009800050519      *---------------------------------------------------------------*
009900050519
010000050519     c     *entry        plist
010100050519     c                   parm                    KPJBA
010200050519     c                   movel     KPJBU         FIDN06ds
010300050519      *
010400050519      * Reperisco dati job
010500050519     c                   exsr      DatiJob
010600050519      *
010700050519      *?Impostazione campi in stampa dell'Apertura Evento?
010800050519     c                   exsr      AE_PrepOut
010900050519      *
011000050519sel 1c                   select
011100050519      *?Stampa della lettera di Apertura Evento?
011200050530w   1c                   when      §STD4flg =  'L'
011300050519     c                   exsr      AE_Stampa
011400050519      *?Invio via e-mail della lettera di Apertura Evento?
011500050530w   1c                   when      §STD4flg =  'M'
011600050519     c                   exsr      AE_Invio
011700050519e   1c                   endsl
011800050519      *
011900050519      * Fine
012000050519     c                   clear                   TIBS02ds
012100050519     c                   movel     'C'           T02tla
012200981029     c                   call      'TIBS02R'
012300050519     c                   parm                    KPJBA
012400050519     c                   parm                    TIBS02ds
012500050519      *
012600050519     c                   eval      *inLR = *on
012700050519      *
012800050519      *---------------------------------------------------------------*
012900050519      *?Reperimento Dati del job (Utente/Operativi)                  ?*
013000050519      *---------------------------------------------------------------*
013100050519     c     DatiJob       BegSr
013200050519      *
013300050519     c     *dtaara       define    §azute        AZUTEds
013400050519     c     *dtaara       define    §datiute      dDatiUte
013500050519      *
013600050519     c                   in(E)     *dtaara
013700050519     c                   IF        %ERROR or RSUT = *blanks
013800050519     c                   clear                   Tibs34Ds
013900050519     c                   call      'TIBS34R'
014000050519     c                   parm                    Tibs34Ds
014100050519     c                   in        *dtaara
014200050519     c                   ENDIF
014300050519      *
014400050519     c                   EndSr
014500050519
014600050519      *---------------------------------------------------------------*
014700050519      *?Preparazione della lettera (da stampare o inviare via e-mail)?*
014800050519      *---------------------------------------------------------------*
014900050519     c     AE_PrepOut    BEGSR
015000050519      *
015100050519     c                   clear                   DN06PI1
015200050519     c                   clear                   DN06PO1
015300050519     c*** no campi:      clear                   DN06PT1
015400050519     c*** no campi:      clear                   DN06P00
015500050519     c                   clear                   DN06PG1
015600050519     c                   clear                   DN06PD1
015700050519      *
015800050519      * Reperimento dati di testata dell'evento
015900050519     c     K02DET01      chain     FNDET000
016000050519     c                   if        NOT %found(FNDET01L)
016100050519     c                   clear                   FNDETds
016200050519     c                   endif
016300050519      *
016400050519      * Decodifica P.O. comunicazione evento
016500050519     c     DETfce        chain     AZORG
016600050519     c                   if        NOT %found(AZORG01L)
016700050519     c                   clear                   ORGdes
016800050519     c                   clear                   ORGind
016900050519     c                   clear                   ORGcpf
017000050519     c                   clear                   ORGloc
017100050519     c                   clear                   ORGpro
017200050519     c                   clear                   ORGtel
017300050519     c                   clear                   ORGfax
017400050519     c                   endif
017500050519      *
017600050519      * Reperimento tab. "STD/4" = Dati AIG
017700050519     c                   reset                   dSTD4
017800050519     c                   reset                   TIBS02ds
017900050519     c                   movel     KNSIF         T02sif
018000050519     c                   movel     'STD'         T02cod
018100050519     c                   movel(p)  '4'           T02ke1
018200050519     c                   call      'TIBS02R'
018300050519     c                   parm                    KPJBA
018400050519     c                   parm                    TIBS02ds
018500050519    4c                   if        T02err = *blanks
018600050519     c                   movel     T02uni        dSTD4
018700050519     c                   endif
018800050519      *
018900050519      * Reperimento tab. "TAD" = Tipo Anomalia Danni
019000050519     c                   reset                   dTAD
019100050519     c                   reset                   TIBS02ds
019200050519     c                   movel     KNSIF         T02sif
019300050519     c                   movel     'TAD'         T02cod
019400050519     c                   movel(p)  DETtad        T02ke1
019500050519     c                   call      'TIBS02R'
019600050519     c                   parm                    KPJBA
019700050519     c                   parm                    TIBS02ds
019800050519    4c                   if        T02err = *blanks
019900050519     c                   movel     T02uni        dTAD
020000050519     c                   endif
020100050519      *
020200050519      * Intestazione
020300050519     c                   eval      PICrag = §STD4rsc
020400050530     c                   eval      PICrco = §STD4rco
020500050519     c                   eval      PICind = §STD4ind
020600050519     c                   eval      PICcap = §STD4cap
020700050519     c                   eval      PICloc = §STD4loc
020800050519     c                   eval      PICpro = §STD4prv
020900050519     c***                eval      PICnaz = §STD4naz
021000050519      *
021100050519     c                   eval      PICdal = 'Bologna, '
021200050519     c                                    + %editc(*date:'Y')
021300050519      *
021400050519      * Oggetto
021500050519     c                   eval      PO1nev = I06nev
021600050519     c                   reset                   WLBdat
021700050519     c                   eval      G02inv = (I06aae * 10000) + DETmge
021800050519     c                   call      'XSRDA8'
021900050519     c                   parm                    WLBdat
022000050519     c                   z-add     G02dat        PO1dev
022100050519      *
022200050519      * Dettaglio/Piede
022300050519     c                   eval      PD1pod = ORGdes
022400050519     c                   eval      PD1poi = ORGind
022500050519     c                   eval      PD1poc = ORGcpf
022600050519     c                   eval      PD1pol = ORGloc
022700050519     c                   eval      PD1pop = ORGpro
022800050523     c                   eval      PD1pot = ORGtel
022900050523     c                   eval      PD1pof = ORGfax
023000050519      *
023100050519     c                   eval      PD1rp  = ORGd10
023200050519      *
023300050519     c                   ENDSR
023400050519
023500050519      *---------------------------------------------------------------*
023600050519      *?Stampa della lettera                                         ?*
023700050519      *---------------------------------------------------------------*
023800050519     c     AE_Stampa     BEGSR
023900050519      *
024000050519     c                   open      FIDN06P1
024100050519      *
024200050519      * Intestazione
024300050520sel 1c                   select
024400050520w   1c                   when      §STD4flg =  'L'
024500050519     c                   write     DN06PI1
024600050520w   1c                   when      §STD4flg =  'M'
024700050520     c                   write     DN06PI2
024800050520e   1c                   endsl
024900050519      *
025000050519      * Oggetto
025100050520sel 1c                   select
025200050520w   1c                   when      §STD4flg =  'L'
025300050519     c                   write     DN06PO1
025400050520w   1c                   when      §STD4flg =  'M'
025500050520     c                   write     DN06PO2
025600050520e   1c                   endsl
025700050519      *
025800050519      * Testo
025900050520sel 1c                   select
026000050520w   1c                   when      §STD4flg =  'L'
026100050520if  2c                   if        *in01 = *on
026200050519     c                   write     DN06PSP
026300050519     c                   eval      *in01 = *off
026400050520e   2c                   endif
026500050519     c                   write     DN06PT1
026600050520w   1c                   when      §STD4flg =  'M'
026700050520     c                   write     DN06PT2
026800050520e   1c                   endsl
026900050519      * - Anomalia
027000050519if  1c                   if        §TADdest <> *blanks
027100050519     c                   eval      PG1tx1   =  'Anomalia: ' + §TADdest
027200050519x   1c                   else
027300050519     c                   eval      PG1tx1   =  'Anomalia: ' + §TADdesc
027400050519     c                   endif
027500050520sel 1c                   select
027600050520w   1c                   when      §STD4flg =  'L'
027700050520if  2c                   if        *in01 = *on
027800050519     c                   write     DN06PSP
027900050519     c                   eval      *in01 = *off
028000050520e   2c                   endif
028100050519     c                   write     DN06PG1
028200050520w   1c                   when      §STD4flg =  'M'
028300050520     c                   write     DN06PG2
028400050520e   1c                   endsl
028500050519      * - Descrizione/i
028600050523      *   (saranno "allineate" alla decodifica dell'anomalia; quindi
028700050523      *   partiranno dall'unicesima posizione)
028800050523      * Anomalia: xxxxxxxxx
028900050523      * 1234567890...
029000050519     c                   clear                   PG1tx1
029100050519     c                   eval      DCStpd   =  'E'
029200050519     c                   move      DETaae        wKEVaae
029300050519     c                   move      DETnev        wKEVnev
029400050519     c                   movel(p)  DS_KEV        DCSnkt
029500050519     c                   clear                   DCSstd
029600050519     c                   clear                   DCSdim
029700050519     c                   clear                   DCShim
029800050519     c                   clear                   DCSnks
029900050519     c                   eval      DCStrc   =  'D'
030000050519     c     K07DCS01      setll     FNDCS000
030100050519     c     K07DCS01      reade     FNDCS000
030200050519do  1c                   DOW       NOT %eof(FNDCS01L)
030300050519sel 2c                   select
030400050519w   2c                   when      DCSatb   <> *blanks
030500050519w   2c                   when      PG1tx1   =  *blanks
030600050523     c***                movel(p)  DCSnot        PG1tx1
030700050523     c                   eval      PG1tx1   =  '          ' + DCSnot
030800050519x   2c                   other
030900050519     c                   eval      %subst(PG1tx1 :
031000050523     c                                    %len(DCSnot) + 11 :
031100050519     c                                    %len(DCSnot))
031200050519     c                                      =  DCSnot
031300050520sel 3c                   select
031400050520w   3c                   when      §STD4flg =  'L'
031500050520if  4c                   if        *in01 = *on
031600050519     c                   write     DN06PSP
031700050519     c                   eval      *in01 = *off
031800050520e   4c                   endif
031900050519     c                   write     DN06PG1
032000050520w   3c                   when      §STD4flg =  'M'
032100050520     c                   write     DN06PG2
032200050520e   3c                   endsl
032300050519     c                   clear                   PG1tx1
032400050519e   2c                   endsl
032500050519     c     K07DCS01      reade     FNDCS000
032600050519e   1c                   ENDDO
032700050520if  1c                   if        PG1tx1  <> *blanks
032800050520sel 2c                   select
032900050520w   2c                   when      §STD4flg =  'L'
033000050520if  3c                   if        *in01 = *on
033100050519     c                   write     DN06PSP
033200050519     c                   eval      *in01 = *off
033300050520e   3c                   endif
033400050519     c                   write     DN06PG1
033500050520w   2c                   when      §STD4flg =  'M'
033600050520     c                   write     DN06PG2
033700050520e   2c                   endsl
033800050519e   1c                   endif
033900050519      *
034000050519      * Dettaglio
034100050520sel 1c                   select
034200050520w   1c                   when      §STD4flg =  'L'
034300050520if  2c                   if        *in01 = *on
034400050519     c                   write     DN06PSP
034500050519     c                   eval      *in01 = *off
034600050520e   2c                   endif
034700050519     c                   write     DN06PD1
034800050520w   1c                   when      §STD4flg =  'M'
034900050520     c                   write     DN06PD2
035000050520e   1c                   endsl
035100050519      *
035200050519      * Piede
035300050520sel 1c                   select
035400050520w   1c                   when      §STD4flg =  'L'
035500050520if  2c                   if        *in01 = *on
035600050519     c                   write     DN06PSP
035700050519     c                   eval      *in01 = *off
035800050520e   2c                   endif
035900050519     c                   write     DN06PP1
036000050520w   1c                   when      §STD4flg =  'M'
036100050520     c                   write     DN06PP2
036200050520e   1c                   endsl
036300050519      *
036400050519     c                   close     FIDN06P1
036500050519      *
036600050519     c                   ENDSR
036700050519
036800050519      *---------------------------------------------------------------*
036900050519      *?Invio della lettera via e-mail                               ?*
037000050519      *---------------------------------------------------------------*
037100050519     c     AE_Invio      BEGSR
037200050530
037300050530      * Reperisco dati in tabella mra per OVRTPRTF
037400050530     c                   reset                   TIBS02ds
037500050530     c                   clear                   trtcm1ds
037600050530     c                   movel     KNSIF         T02sif
037700050530     c                   movel     'MRA'         T02cod
037800050530     c                   movel(p)  'FIDN06R1'    T02ke1
037900050530     c                   call      'TIBS02R'
038000050530     c                   parm                    KPJBA
038100050530     c                   parm                    TIBS02ds
038200050530    4c                   if        T02err = *blanks
038300050530     c                   movel     T02uni        dmradan
038400050530     c                   eval      §cm1dst = §mraddest
038500050530     c                   move      dutpou        §cm1po
038600050530     c                   move      '0'           §cm1sts
038700050530     c                   movel     §mradreg      §cm1tips
038800050530     c                   movel     §mradidpro    §cm1idp
038900050530     c                   eval      §cm1mitt = §mradmitt
039000050530     c                   eval      §cm1var  =  '*OBJM*Apertura EVENTO N. '
039100050530     c                                      +  %trim(%editc(DETnev:'Z'))
039200050530     c                                      +  ' Anno '
039300050530     c                                      +  %char(DETaae)
039400050530      *
039500050530      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LA LETTERA DA INVIARE
039600050530      *   TRAMITE E-mail all'assicurazione
039700050530      *
039800050530
039900050530      * eseguo ovprtf
040000050601     c                   eval      %subst(cmm(1): 23: 10) = §MRADOUTQI
040100050530     c                   eval      qcmd = %trim(cmm(1)) + trtcm1ds
040200050530     c                             + ''')'
040300050530     c                   z-add     395           Qlen
040400050530     c                   CALL      'QCMDEXC'                            90
040500050530     c                   PARM                    Qcmd
040600050530     c                   PARM                    Qlen
040700050530     c
040800050530      *
040900050519      *
041000050519      * Eseguo la stampa da inviare
041100050519     c                   exsr      AE_Stampa
041200050520      * DltOvr
041300050530     c                   clear                   Qcmd
041400050519     c                   call      'QCMDEXC'                            90
041500050520     c                   parm      C_Cmd_1       Qcmd
041600050520     c                   parm      60            Qlen
041700050519      *
041800050530     c                   endif
041900050519      *
042000050519     c                   ENDSR
042100050530**
042200050530OVRPRTF FIDN06P1 OUTQ(          ) USRDFNDTA('
