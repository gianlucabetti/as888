000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060615      **?-------------------------------------------------------------------*****
000500060613      *  Da UPLOAD                                                              *
000600161019      *  TRASCODIFICA : esegue un giro sugli SCAN in ATTESA  (parcheggiati)     *
000700161019      *               Servono per dare per CONSEGNATE le spedizioni che         *
000800161019      *               non ricevono lo SCAN di DELIVERY dopo lo SCAN del MIC.    *
000900060615      **?-------------------------------------------------------------------*****
001000060719     FTidp502L  uF   E           K DISK    usropn
001100991124      *
001200161018      **?-------------------------------------------------------------------*****
001300000223     D W0140           S             14  0
001400991129     D WORA            S              6  0
001500991129     D WDTGIO          S              8  0
001600991129     D DATEU           S              8  0
001700060720     D DATamg          S              8  0
001800991129     D DATA_eur        S               D   DATFMT(*eur)
001900060720     D DATA_amg        S               D   DATFMT(*iso)
002000160120     D DATA_iso        S               D   DATFMT(*iso)
002100160120      * ?------------------------------------------------------------------ */
002200160120     D   Digits        C                   '0123456789'
002300060614      **?------------------------------------------------------------------ */
002400060614     C*? Ds Decodifica dei campi
002500161019      **?------------------------------------------------------------------ */
002600161130     D FIEU18        E DS                  EXTNAME(FIEU18DS)
002700060615      * ?______________________________________________________________     */
002800060612      *   Ciclo principale
002900060615      * ?______________________________________________________________     */
003000161019     c     *ENTRY        PLIST
003100161019     C                   parm                    esito             1
003200161019      *
003300161019      *
003400161019      * Recupero data e ora
003500161019     C                   TIME                    WORA
003600161019     C                   TIME                    W0140
003700161019      * UDATE IN GGMMAAAA
003800161019     C                   MOVE      W0140         WDTGIO
003900161019      * UDATE IN AAAAMMGG
004000161019     C     *eur          MOVEL     WDTGIO        DATA_eur
004100161019     C     *iso          MOVEL     DATA_eur      dateu
004200060719      *
004300161019     c                   SETON                                        LR
004400161019      * ? esegue un ciclo sugli SCAN in attesa x dare come consegnate le
004500161019      * ?spedizioni di cui si è ricevuto il MIC e non lo SCAN di consegna.
004600060719      * ?_______________________________________________________________    */
004700060719     C*? ESEGUO ciclo sugli SCAN in ATTESA x chiudere spedizioni come CONSEGNATE
004800060719      * ?_______________________________________________________________    */
004900060719     c                   open      tidp502L
005000060719      *
005100060719      * Dalla data scadenza più bassa
005200060719     c     *START        setll     tidp502L
005300060719     c                   read      tidp502L
005400060719      *
005500060719     c                   dow       not %EoF(tidp502L)
005600060719      *
005700060719      *  se la data scadenza (GG di attesa) è superiore all'UDATE esce dal ciclo
005800060719     c                   movel     DP5DATAGG     Data08            8 0
005900060719     c                   if        Data08 > DATEU
006000060719     c                   leave
006100060719     c                   end
006200060719      *
006300060719      * Imposta il record da passare al pgm di elaborazione i modalità (A)
006400060719      *   elaborazione SCAN di ATTESA.
006500161130     c                   clear                   FIEU18
006600060719      *
006700060719     c                   eval      I5PARCEL    =    DP5PARCEL
006800060719     c                   eval      I5SCANT     =    DP5SCANT
006900060719     c                   eval      I5DEPCODE   =    DP5DEPCODE
007000060719     c                   eval      I5DEPNAME   =    DP5DEPNAME
007100060808     c                   eval      I5DATTIM    =    DP5DATTIM                   <<----
007200060719     c                   eval      I5ROUTE     =    DP5ROUTE
007300060719     c                   eval      I5TOUR      =    DP5TOUR
007400060719     c                   eval      I5PCODE     =    DP5PCODE
007500060719     c                   eval      I5SERVICE   =    DP5SERVICE
007600060719     c                   eval      I5CCOUNTRY  =    DP5COUNTRY
007700060719     c                   eval      I5CONSZIP   =    DP5CONSZIP
007800060719     c                   eval      I5ADDSER1   =    DP5ADDSER1
007900060719     c                   eval      I5ADDSER2   =    DP5ADDSER2
008000060719     c                   eval      I5ADDSER3   =    DP5ADDSER3
008100060719     c                   eval      I5WEIGHT    =    DP5WEIGHT
008200060719     c                   eval      I5CUSTREF   =    DP5CUSTREF
008300060719     c                   eval      I5PODIMAG   =    DP5PODIMAG
008400060719     c                   eval      I5RECNAME   =    DP5RECNAME
008500060719     c                   eval      I5INFOTXT   =    DP5INFOTXT
008600060719      *
008700060719     c                   move      'A'           ok_18             1
008800060719     c                   clear                   msgerr           80
008900060719     c                   call      'FIEU18R'
009000161130     c                   parm                    FIEU18
009100060719     c                   parm                    ok_18
009200060719     c                   parm                    msgerr
009300060720      *
009400060720      * calcolo un mese dalla data dell'evento: (serve per eliminare il record
009500060720      *  se ritornato con errori dal FIEU18R)
009600060720     c                   clear                   piu_di_30gg       1
009700060720     c                   movel     dp5DATAGG     datamg            8 0
009800060720     C     *iso          MOVEL     datamg        data_amg
009900060720     c                   adddur    30:*d         data_amg
010000060720     C     *iso          MOVEL     data_amg      datamg
010100060720      *
010200060720      *  Se sono passati + di 30 giorni
010300060720     c                   if        datamg < WDTGIO
010400060720     c                   eval      piu_di_30gg ='S'
010500060720     c                   end
010600060720      *
010700060719      * se non presenti errori cancella lo SCAN di attesa
010800060720      *  oppure dopo un mese di errori lo cancello comunque se persevera la segnalazione
010900060720     c                   if        msgerr = *blank or
011000060720     c                             msgerr <> *blank and piu_di_30gg ='S'
011100060719     c                   delete    tidp500r
011200060719     c                   End
011300060719      *
011400060719     c                   read      tidp502L
011500060719     c                   EndDO
011600060719      *
011700060719     c                   close     tidp502L
011800161019      *
011900161019     c                   RETURN
012000060615      * ?_______________________________________________________________    */
012100060615     C     *INZSR        BEGSR
012200060615      * ?_______________________________________________________________    */
012300160120      *
012400160120      *
012500050414      *
012600991124     C                   ENDSR
012700060615      * ?_______________________________________________________________    */
012800060615     C     *pssr         BEGSR
012900160121      *
013000060614     C                   eval      esito = '2'
013100060614     C*
013200060614     C                   ENDSR     '*CANCL'
013300161019      * ?_______________________________________________________________    */
