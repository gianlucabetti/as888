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
023100050519     c                   ENDSR
023200050519
023300050519      *---------------------------------------------------------------*
023400050519      *?Stampa della lettera                                         ?*
023500050519      *---------------------------------------------------------------*
023600050519     c     AE_Stampa     BEGSR
023700050519      *
023800050519     c                   open      FIDN06P1
023900050519      *
024000050519      * Intestazione
024100050520sel 1c                   select
024200050520w   1c                   when      §STD4flg =  'L'
024300050519     c                   write     DN06PI1
024400050520w   1c                   when      §STD4flg =  'M'
024500050520     c                   write     DN06PI2
024600050520e   1c                   endsl
024700050519      *
024800050519      * Oggetto
024900050520sel 1c                   select
025000050520w   1c                   when      §STD4flg =  'L'
025100050519     c                   write     DN06PO1
025200050520w   1c                   when      §STD4flg =  'M'
025300050520     c                   write     DN06PO2
025400050520e   1c                   endsl
025500050519      *
025600050519      * Testo
025700050520sel 1c                   select
025800050520w   1c                   when      §STD4flg =  'L'
025900050520if  2c                   if        *in01 = *on
026000050519     c                   write     DN06PSP
026100050519     c                   eval      *in01 = *off
026200050520e   2c                   endif
026300050519     c                   write     DN06PT1
026400050520w   1c                   when      §STD4flg =  'M'
026500050520     c                   write     DN06PT2
026600050520e   1c                   endsl
026700050519      * - Anomalia
026800050519if  1c                   if        §TADdest <> *blanks
026900050519     c                   eval      PG1tx1   =  'Anomalia: ' + §TADdest
027000050519x   1c                   else
027100050519     c                   eval      PG1tx1   =  'Anomalia: ' + §TADdesc
027200050519     c                   endif
027300050520sel 1c                   select
027400050520w   1c                   when      §STD4flg =  'L'
027500050520if  2c                   if        *in01 = *on
027600050519     c                   write     DN06PSP
027700050519     c                   eval      *in01 = *off
027800050520e   2c                   endif
027900050519     c                   write     DN06PG1
028000050520w   1c                   when      §STD4flg =  'M'
028100050520     c                   write     DN06PG2
028200050520e   1c                   endsl
028300050519      * - Descrizione/i
028400050523      *   (saranno "allineate" alla decodifica dell'anomalia; quindi
028500050523      *   partiranno dall'unicesima posizione)
028600050523      * Anomalia: xxxxxxxxx
028700050523      * 1234567890...
028800050519     c                   clear                   PG1tx1
028900050519     c                   eval      DCStpd   =  'E'
029000050519     c                   move      DETaae        wKEVaae
029100050519     c                   move      DETnev        wKEVnev
029200050519     c                   movel(p)  DS_KEV        DCSnkt
029300050519     c                   clear                   DCSstd
029400050519     c                   clear                   DCSdim
029500050519     c                   clear                   DCShim
029600050519     c                   clear                   DCSnks
029700050519     c                   eval      DCStrc   =  'D'
029800050519     c     K07DCS01      setll     FNDCS000
029900050519     c     K07DCS01      reade     FNDCS000
030000050519do  1c                   DOW       NOT %eof(FNDCS01L)
030100050519sel 2c                   select
030200050519w   2c                   when      DCSatb   <> *blanks
030300050519w   2c                   when      PG1tx1   =  *blanks
030400050523     c***                movel(p)  DCSnot        PG1tx1
030500050523     c                   eval      PG1tx1   =  '          ' + DCSnot
030600050519x   2c                   other
030700050519     c                   eval      %subst(PG1tx1 :
030800050523     c                                    %len(DCSnot) + 11 :
030900050519     c                                    %len(DCSnot))
031000050519     c                                      =  DCSnot
031100050520sel 3c                   select
031200050520w   3c                   when      §STD4flg =  'L'
031300050520if  4c                   if        *in01 = *on
031400050519     c                   write     DN06PSP
031500050519     c                   eval      *in01 = *off
031600050520e   4c                   endif
031700050519     c                   write     DN06PG1
031800050520w   3c                   when      §STD4flg =  'M'
031900050520     c                   write     DN06PG2
032000050520e   3c                   endsl
032100050519     c                   clear                   PG1tx1
032200050519e   2c                   endsl
032300050519     c     K07DCS01      reade     FNDCS000
032400050519e   1c                   ENDDO
032500050520if  1c                   if        PG1tx1  <> *blanks
032600050520sel 2c                   select
032700050520w   2c                   when      §STD4flg =  'L'
032800050520if  3c                   if        *in01 = *on
032900050519     c                   write     DN06PSP
033000050519     c                   eval      *in01 = *off
033100050520e   3c                   endif
033200050519     c                   write     DN06PG1
033300050520w   2c                   when      §STD4flg =  'M'
033400050520     c                   write     DN06PG2
033500050520e   2c                   endsl
033600050519e   1c                   endif
033700050519      *
033800050519      * Dettaglio
033900050520sel 1c                   select
034000050520w   1c                   when      §STD4flg =  'L'
034100050520if  2c                   if        *in01 = *on
034200050519     c                   write     DN06PSP
034300050519     c                   eval      *in01 = *off
034400050520e   2c                   endif
034500050519     c                   write     DN06PD1
034600050520w   1c                   when      §STD4flg =  'M'
034700050520     c                   write     DN06PD2
034800050520e   1c                   endsl
034900050519      *
035000050519      * Piede
035100050520sel 1c                   select
035200050520w   1c                   when      §STD4flg =  'L'
035300050520if  2c                   if        *in01 = *on
035400050519     c                   write     DN06PSP
035500050519     c                   eval      *in01 = *off
035600050520e   2c                   endif
035700050519     c                   write     DN06PP1
035800050520w   1c                   when      §STD4flg =  'M'
035900050520     c                   write     DN06PP2
036000050520e   1c                   endsl
036100050519      *
036200050519     c                   close     FIDN06P1
036300050519      *
036400050519     c                   ENDSR
036500050519
036600050519      *---------------------------------------------------------------*
036700050519      *?Invio della lettera via e-mail                               ?*
036800050519      *---------------------------------------------------------------*
036900050519     c     AE_Invio      BEGSR
037000050530
037100050530      * Reperisco dati in tabella mra per OVRTPRTF
037200050530     c                   reset                   TIBS02ds
037300050530     c                   clear                   trtcm1ds
037400050530     c                   movel     KNSIF         T02sif
037500050530     c                   movel     'MRA'         T02cod
037600050530     c                   movel(p)  'FIDN06R1'    T02ke1
037700050530     c                   call      'TIBS02R'
037800050530     c                   parm                    KPJBA
037900050530     c                   parm                    TIBS02ds
038000050530    4c                   if        T02err = *blanks
038100050530     c                   movel     T02uni        dmradan
038200050530     c                   eval      §cm1dst = §mraddest
038300050530     c                   move      dutpou        §cm1po
038400050530     c                   move      '0'           §cm1sts
038500050530     c                   movel     §mradreg      §cm1tips
038600050530     c                   movel     §mradidpro    §cm1idp
038700050530     c                   eval      §cm1mitt = §mradmitt
038800050530     c                   eval      §cm1var  =  '*OBJM*Apertura EVENTO N. '
038900050530     c                                      +  %trim(%editc(DETnev:'Z'))
039000050530     c                                      +  ' Anno '
039100050530     c                                      +  %char(DETaae)
039200050530      *
039300050530      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LA LETTERA DA INVIARE
039400050530      *   TRAMITE E-mail all'assicurazione
039500050530      *
039600050530
039700050530      * eseguo ovprtf
039800050601     c                   eval      %subst(cmm(1): 23: 10) = §MRADOUTQI
039900050530     c                   eval      qcmd = %trim(cmm(1)) + trtcm1ds
040000050530     c                             + ''')'
040100050530     c                   z-add     395           Qlen
040200050530     c                   CALL      'QCMDEXC'                            90
040300050530     c                   PARM                    Qcmd
040400050530     c                   PARM                    Qlen
040500050530     c
040600050530      *
040700050519      *
040800050519      * Eseguo la stampa da inviare
040900050519     c                   exsr      AE_Stampa
041000050520      * DltOvr
041100050530     c                   clear                   Qcmd
041200050519     c                   call      'QCMDEXC'                            90
041300050520     c                   parm      C_Cmd_1       Qcmd
041400050520     c                   parm      60            Qlen
041500050519      *
041600050530     c                   endif
041700050519      *
041800050519     c                   ENDSR
041900050530**
042000050530OVRPRTF FIDN06P1 OUTQ(          ) USRDFNDTA('
