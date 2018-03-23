000100151015      * ?_________________________________________________________________  */
000200151015      * ?Driver per restituire la tabella LPD sulla base della LNP passata  */
000300151015      * ?ma soprattutto per restituire in Bollettazione la tariffa necessa  */
000400151015      * ?ria in IMPORT.                                                     */
000500151015      * ?In import dei dati GEODATA per tradurre le bolle servono le info   */
000600151015      * ?della tab. LPD x impostare il VAB/EDIVAB.                          */
000700151015      * ?Allo stesso modo durante un RESO DPD per la bolla figlia.          */
000800151015      * ?_________________________________________________________________  */
000900990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
001000151015      * ?_________________________________________________________________  */
001100151015     FazORG01L  IF   E           K DISK
001200151015     FTNCLD00F  IF   E           K DISK
001300151015     ftntbe01l  if   e           k disk
001400151015      * ?_________________________________________________________________  */
001500151015     d fieuLPDds     e ds
001600151015     D dLPD          e ds
001700151015     D og143         e ds
001800151015      * ?_________________________________________________________________  */
001900151015     D W0140           S             14  0
002000151015     D WORA            S              6  0
002100151015     D XX              S              3  0 INZ
002200151015     D WDTGIO          S              8  0
002300151015     D DATEU           S              8  0
002400151015     D DATA_eur        S               D   DATFMT(*eur)
002500151015     D DATA_iso        S               D   DATFMT(*iso)
002600151015     D Linea           S              3S 0
002700151015      * ?-----------------------------------------------------------------  */
002800150723     D   Digits        C                   '0123456789'
002900151015      * ?_________________________________________________________________  */
003000151015      *
003100151015      *       EULPD000
003200151016      *       ILPDLNP        1     3 S  0 LINEA PARTENZA    (x LPD)
003300151016      *       ILPDPARCEL     4    14 A    NUM.PARCEL DPD    (x VTG)
003400151016      *       ILPDLNA       18     3 S  0 LINEA DI ARRIVO   (x LHS)
003500151016      *       ILPDHUBP      21     4 A    HUB   DI PARTENZA (x ORG)
003600151016      *       ILPDDEPP      25     4 A    DEPOT DI PARTENZA (x ORG)
003700151016      *       ILPDHUBA      29     4 A    HUB   DI ARRIVO   (x ORG)
003800151016      *       ILPDDEPA      33     4 A    DEPOT DI ARRIVO   (x ORG)
003900151015      *       OLPDTIP       37     1 A    TIPOLOGIA TARIFFA
004000151016      *       OLPDTAR       38     3 S  0 TARIFFA BOLLA ONE
004100151016      *       OLPDTRE       41     3 S  0 TARIFFA RESO  ONE
004200151016      *       OLPDUNI       44   256 A    DS LPD COMPLETA
004300151020      *       OLPDERR      300     1 A    "E"= NON TROVATO e TORNA 190 x DEFAULT
004400151015      *
004500150723     c                   seton                                        lr
004600151015      *
004700150723      * primi 4 caratteri del Parcel contengono il Depot di origine
004800150723      * del Parcel.
004900151015     c                   movel     ilpdPARCEL    Origin_Depot      4
005000151015      *
005100151015      * -----------------                  -------------------
005200151015      ** imposta il Depot di provenienza x testare se VTG
005300151015     C                   movel(p)  'VTG'         cldCOD
005400151015     C                   move      Origin_Depot  cldDEP
005500151015     c     key_VTG       setll     tncld00F
005600151015     c     key_VTG       reade     tncld00F
005700151015     c                   dow       not %EoF(tncld00F)
005800151015     c                   if        dateu >= cldDDA and
005900151015     c                             dateu <= cldADA
006000151015      **  Se è un collo VTG
006100151015      **     deve avere una tariffa differente
006200151015     c                   movel     'V'           OLPDTIP
006300151015     c                   leave
006400151015     c                   end
006500151015     c     key_VTG       reade     tncld00F
006600151015     c                   endDO
006700151015      *
006800151015      * -----------------                  -------------------
006900151020     c                   clear                   HUBdpd            1
007000151015      * Partenza OBBLIGATORIO
007100151015     c                   if        ilpdHUBP = *blank  and
007200151015     c                             ilpdDEPP = *blank
007300151015      * aggancia ORGANIGRAMMA
007400151020     c                   clear                   og143
007500151015     c     ilpdLNP       CHAIN     azORG01L
007600151015     c                   if        %Found(azORG01L)
007700151015     c                   eval      og143 = orgDE3
007800151020     c                   if        §OGntw = 'DPD'
007900151020     c                   move      'P'           HUBdpd
008000151020     c                   end
008100151015     c                   movel     §OGHB1        ilpdHUBP
008200151015     c                   movel     §OGDP1        ilpdDEPP
008300151020     c                   end
008400151020      *
008500151020      * se non trovato nulla defalt "190"
008600151020     c                   if        not %Found(azORG01L)  or
008700151020     c                              §OGHB1 = *blank and §OGDP1 = *blank
008800151020     c                   eval      OlpdERR = 'E'
008900151020     c                   eval      ilpdLNP = 190
009000151020     c                   end
009100151020      *
009200151023     c                   else
009300151023     c                   if        ilpdHUBP <> *blank
009400151023     c                   move      'P'           HUBdpd
009500151023     c                   end
009600151015     c                   end
009700151015      *
009800151015      * Arrivo   FACOLTATIVO
009900151015     c                   if        ilpdHUBA = *blank  and
010000151015     c                             ilpdDEPA = *blank
010100151015     c                                and
010200151016     c                             ilpdLNA  > 0
010300151015      * aggancia ORGANIGRAMMA
010400151020     c                   clear                   og143
010500151015     c     ilpdLNA       CHAIN     azORG01L
010600151015     c                   if        %Found(azORG01L)
010700151015     c                   eval      og143 = orgDE3
010800151020     c                   if        §OGntw = 'DPD'
010900151020     c                   move      'A'           HUBdpd
011000151020     c                   end
011100151015     c                   movel     §OGHB1        ilpdHUBA
011200151015     c                   movel     §OGDP1        ilpdDEPA
011300151015     c                   end
011400151023     c                   else
011500151023     c                   if        ilpdHUBA <> *blank
011600151023     c                   move      'A'           HUBdpd
011700151023     c                   end
011800151015     c                   end
011900151015      *
012000151016      *  mi trovo in condizioni di LHS se:  (comunque devo avere la LNA)
012100151015      *   il depot di partenza coincide con la HUB di arrivo o
012200151015      *   il depot di arrivo   coincide con la HUB di partenza.
012300151016     c                   IF        ilpdLNA  > 0
012400151015      *
012500151015     c                   if        ilpdHUBA = ilpdDEPP  or
012600151015     c                             ilpdHUBP = ilpdDEPA
012700151015      *   se LHS + VTG
012800151015     c                   if         OLPDTIP = 'V'
012900151015     c                   eval      OLPDTIP = 'X'
013000151016     c                   else
013100151016      *   se LHS
013200151016     c                   eval      OLPDTIP = 'L'
013300151015     c                   end
013400151015      *
013500151015     c                   end
013600151015      *
013700151016     c                   endIF
013800151016      *
013900151016      * imposta l'Errore del driver.
014000151020     c                   if          HUBdpd ='P'
014100151016     c                   z-add     ilpdLNP       Linea
014200151020     c                   elseif      HUBdpd ='A'
014300151020     c                   z-add     ilpdLNA       Linea
014400151020     c                   end
014500151016     c                   exsr      tabella_LPD
014600151016      *
014700151016     c                   if        ilpdHUBA = *blank  and
014800151016     c                             ilpdHUBP = *blank
014900151020     c                   eval      OlpdERR = 'E'
015000151016     c                   endIF
015100151016      *
015200150723     c                   RETURN
015300151015      * ?_________________________________________________________________  */
015400051202     c     *inzsr        begsr
015500151015      * ?_________________________________________________________________  */
015600051202     c     *entry        plist
015700151015     c                   parm                    fieulpdds
015800150723      *   campi di output
015900151015     c                   clear                   OLPDTAR
016000151015     c                   clear                   OLPDTRE
016100151015     c                   clear                   OLPDUNI
016200151015      * Std. tipo
016300151015     c                   movel     'S'           OLPDTIP
016400151015      *
016500151015     C     key_VTG       KLIST
016600151015     C                   KFLD                    cldCOD
016700151015     C                   KFLD                    cldDEP
016800151015      *
016900151015     C     KTBE          KLIST
017000151015     C                   KFLD                    TBeCOD
017100151015     C                   KFLD                    TBeKe1
017200151015     C                   KFLD                    TBeKe2
017300151015      *
017400151015      * Recupero data e ora
017500151015     C                   TIME                    WORA
017600151015     C                   TIME                    W0140
017700151015      * UDATE IN GGMMAAAA
017800151015     C                   MOVE      W0140         WDTGIO
017900151015      * UDATE IN AAAAMMGG
018000151015     C     *eur          MOVEL     WDTGIO        DATA_eur
018100151015     C     *iso          MOVEL     DATA_eur      dateu
018200150723      *
018300060628     c                   endsr
018400151015      * ?_________________________________________________________________  */
018500151015     C     tabella_LPD   BEGSR
018600151015      * ?_________________________________________________________________  */
018700151020     c     default       tag
018800151015     c                   clear                   dLPD
018900151015     c                   movel(p)  'LPD'         tbecod
019000151015     c                   movel(p)  linea         tbeke1
019100151015     c                   clear                   tbeke2
019200151015     C     KTBE          chain     tntbe01l
019300151015      * ha trovato i dati necessari per instradare il record della spedizione
019400151015      * alla filiale di partenza
019500151015     c                   if        %Found(tntbe01l)
019600151015     c                   eval         dLPD = tbeUNI
019700151016     c                   eval      OlpdUNI = tbeUNI
019800151020      *
019900151016     c                   IF        OLPDTIP = 'V'
020000151016     c                   eval      OlpdTAR =  §LPDTARVTG
020100151016     c                   eval      OlpdTRE =  §LPDTREVTG
020200151016     c                   elseIF    OLPDTIP = 'L'
020300151016     c                   eval      OlpdTAR =  §LPDTARLHS
020400151016     c                   eval      OlpdTRE =  §LPDTRELHS
020500151016     c                   elseIF    OLPDTIP = 'X'
020600151016     c                   eval      OlpdTAR =  §LPDTARLHV
020700151016     c                   eval      OlpdTRE =  §LPDTRELHV
020800151016     c                   else
020900151016      *   è lo standard (S)
021000151016     c                   eval      OlpdTAR =  §LPDTAR
021100151016     c                   eval      OlpdTRE =  §LPDTRE
021200151016     c                   end
021300151016      *
021400151016      * se non trovata la LPD
021500151016     c                   else
021600151016     c                   clear                   ilpdHUBA
021700151016     c                   clear                   ilpdHUBP
021800151020     c                   eval      OlpdERR = 'E'
021900151020     c                   eval      linea   = 190
022000151020     c                   goto      default
022100151016     c                   end
022200151015      *
022300151015     c                   Endsr
022400151016      * ?_________________________________________________________________  */
022500170406      *       X non bloccare in nessun caso il traduttore CLIENTI
022600170406      * ?_________________________________________________________________  */
022700170406     C     *pssr         BEGSR
022800170406     C
022900170406      **
023000170406     c                   eval       OLPDUNI = 'Problemi decodifica Depot-
023100170406     c                              tabella LPD'
023200170406     c                   eval       OLPDERR = 'E'
023300170406      **
023400170406     C                   ENDSR     '*CANCL'
023500170406      * ?------------------------------------------------------------------ */
