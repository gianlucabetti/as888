000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200050707     H DECEDIT('0.') DATEDIT(*YMD.)
000300140220     H* TRTCT95R         ---------------------------------------------------*
000400140221      **?__________________________________________________________________ */
000500140225      *                                                                     *
000600140225      **?__A T T E N Z I O N E :  si deve fare in fretta a farlo partire___ */
000700140225      **?__________________________________________________________________ */
000800140225      *                                                                     *
000900140225      *   VUOLE SOLO gli stati di CONSEGNA (non USIAMO l'EDSTS00F)          *
001000140225      *                                                                     *
001100140221      *     x fare la conversione ADESSO....forse in futuro.                *
001200140225      *                                                                     *
001300140225      *                                                                     *
001400140225      **?______FACCIAMO una pura CONVERSIONE da righe IFTSTA a REPORT______ */
001500140317      **?__________________________________________________________________ */
001600140317      *                                                                     *
001700140317      * MODIFICHE di affinamento del traduttore:                            *
001800140317      *  1)  la data senza orario deve essere impostata "0000"  e non con   *
001900140317      *      la tipologia (":102") (vedi caso STS XXX+XXX)                  *
002000140317      *                                                                     *
002100140317      **  2) le MANCANZE e RESI PARTNER sono da considerare come STATI      *
002200140317      *   di chiusura della spedizione oltre al "21" Consegnato OK          *
002300140317      *   considero anche il "14+117" Perduto e "14+249" Reso Automatico    *
002400140317      *   ed aggiungo il Consegnato Parzialmente "23+117" e "23"            *
002500110519      **?__________________________________________________________________ */
002600140220      *                                                                     *
002700140220      * Scrittura del REPORT:2:3:GT:GTF   x CEVA/HAGER (IFTSTA)             *
002800140221      *                                                                     *
002900140221      *   così formattato:                                                  *
003000140221      *            UNA:+.? '                                                *
003100140221      *            UNB+UNOA:2+ITBAR005:ZZ+HAGER:22+140219:2003+2889'        *
003200140221      *            UNH+1+REPORT:2:3:GT:GTF'                                 *
003300140221      *            BGM+784+20030419022014+9'                                *
003400140221      *            NAD+FW+04507990150+BRT SPA?                              *
003500140221      *            UNS+D'                                                   *
003600140221      *            RFF+SEN+21400142'                                        *
003700140221      *            RSJ+FW+LIV+CFM'                                          *
003800140225      *            DTM+DSJ+140113+1045'                                     *
003900140221      *            TXT+DEL+++NICOLA'                                        *
004000140221      *            UNS+S'                                                   *
004100140221      *            UNT+10+1'                                                *
004200140221      *            UNH+2+REPORT:2:3:GT:GTF'                                 *
004300140221      *            BGM+784+20030419022014+9'                                *
004400140221      *            NAD+FW+04507990150+BRT SPA?                              *
004500140221      *            UNS+D'                                                   *
004600140221      *            RFF+SEN+21400148'                                        *
004700140221      *            RSJ+FW+LIV+CFM'                                          *
004800140225      *            DTM+DSJ+140113+1045'                                     *
004900140221      *            TXT+DEL+++NICOLA'                                        *
005000140221      *            UNS+S'                                                   *
005100140221      *            UNT+10+2'                                                *
005200140221      *            UNH+3+REPORT:2:3:GT:GTF'                                 *
005300140221      *            BGM+784+20030419022014+9'                                *
005400140221      *            NAD+FW+04507990150+BRT SPA?                              *
005500140221      *            UNS+D'                                                   *
005600140221      *            RFF+SEN+21400223'                                        *
005700140221      *            RSJ+FW+LIV+CFM'                                          *
005800140221      *            DTM+DSJ+140114+1200'                                     *
005900140221      *            UNS+S'                                                   *
006000140221      *            UNT+9+3'                                                 *
006100140221      *            UNZ+3+2889'                                              *
006200140221      *                                                                     *
006300050927      **?__________________________________________________________________ */
006400140220     FTRTCT95W  IF   E           K DISK
006500110519      *
006600140225     ftivgd00f  o  a E             DISK    COMMIT
006700140225      *
006800140225     Ftivir02L  IF   E           K DISK
006900140225     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
007000140225     F                                     PREFIX(f_) usropn
007100110519      **?__________________________________________________________________ */
007200140221     D prmppt          s             50
007300140221     D prmesito        s              1
007400140221     D wrkesito        s                   like(prmesito)
007500140225      **?__________________________________________________________________ */
007600140225     D TipoF           C                   CONST('EC')
007700140225     D dwlprg          s             10    INZ(*all'0')
007800140225     D wrkprg          s              8  0 INZ(*zeros)
007900140225     D Var_ISV         s                   like(Virfi1)
008000140225     d trul47ds      e ds
008100140225      **?__________________________________________________________________ */
008200140225     D progressivo     s             10    inz(' ')
008300140225     D in_generale     s              1    inz(' ')
008400140225      **
008500140225     D Tipo_segmento   s              3
008600140225     D Primo_CNI       s              1
008700130502      **
008800140221     D campo_dati      s                   like(FLATREC)
008900140221      **
009000140225     D UNH_piu         s              4    INZ('UNH+')
009100140225     D UNH_cnt         s              5s 0 INZ(0)
009200140221     D UNH_dx          s             18    INZ('+REPORT:2:3:GT:GTF')
009300140221      **
009400140225     D riga_BGM        s                   like(FLATREC)
009500140225     D salva_STS       s                   like(FLATREC)
009600140221      **
009700140221     D NAD_fw          s             26    INZ('NAD+FW+04507990150+BRT SPA')
009800140225      **
009900140221     D UNS_d           s              5    INZ('UNS+D')
010000140221      **
010100140221     D RFF_SEN         s              8    INZ('RFF+SEN+')
010200140221      **
010300140221     D RSJ_fw          s              7    INZ('RSJ+FW+')
010400140221     D STS_consegnato  s              7    INZ('LIV+CFM')
010500140225     D NON_consegnato  s              7    INZ('XXX+XXX')
010600140317     D PRZ_consegnato  s              7    INZ('LIV+MQP')
010700140317     D MANCANTE        s              7    INZ('SOL+MQD')
010800140317     D RESO_autom      s              7    INZ('SOL+REI')
010900140225      **
011000140225     D DTM_DSJ         s              8    INZ('DTM+DSJ+')
011100140225     D DTM_data        s              6    INZ(' ')
011200140225     D DTM_ora         s              4    INZ(' ')
011300140225      **
011400140225     D TXT_DEL         s              7    INZ('TXT+DEL')
011500140221      **
011600140221     D UNS_s           s              5    INZ('UNS+S')
011700140221      **
011800140225     D UNT_piu         s              4    INZ('UNT+')
011900140225     D UNT_cnt         s              5s 0 INZ(0)
012000140221      **
012100140225     D UNZ_piu         s              4    INZ('UNZ+')
012200140221      **
012300140221     D UNZ_sx          s            100    INZ(' ')
012400140221      **
012500140221     D Position        s              5s 0 INZ(0)
012600140221     D num             s              5s 0 INZ(0)
012700140221     D Da              s              5s 0 INZ(0)
012800140221     D A               s              5s 0 INZ(0)
012900140225
013000140225     d CED_Bart        C                   CONST('CEDAlert@Brt.it;')
013100050927      **?__________________________________________________________________ */
013200050704      *    main
013300110316      **?__________________________________________________________________ */
013400130415     c     *entry        plist
013500140221     C                   parm                    prmppt
013600140221     C     wrkesito      parm      wrkesito      prmesito
013700140225     c*
013800140225     c     kvir02        klist
013900140225     C                   kfld                    virKSC
014000140225     C                   kfld                    virTIP
014100140221      *
014200140221     C                   eval      wrkesito = '0'
014300140225     c*
014400140225     C                   TIME                    ORADAT           14 0
014500140225     C                   MOVEL     ORADAT        ORATR             6 0
014600140225     c                   move      *date         udtymd            8 0
014700140225      * ------
014800140225     C                   exsr      CONVERTE
014900140225      *
015000140225     c                   COMMIT
015100140221      *   chiude in LR
015200140221     c                   seton                                        LR
015300140225      **?__________________________________________________________________ */
015400140225     C     CONVERTE      BEGSR
015500140221     C*
015600140221     C*    per capire solo quando si troverà a scrivere il primo CNI
015700140221     C                   EVAL      Primo_CNI = 'S'
015800140221     C*
015900140221     C*   legge il flat di work e rilascia i records scrivendoli sul VGD
016000140221     C*    trasformati
016100140225     C                   read      TRTCT95W
016200140221     C                   dow       not %eof(TRTCT95W)
016300140225     C*  decod.generali
016400140225     C                   exsr      SOLO_UNA_VOLTA
016500140225     C*
016600140221     c                   eval      tipo_segmento = %subst(flatrec:1:3)
016700140221     c                   exsr      CHK_segmento
016800140221     C*
016900140221     C                   read      TRTCT95W
017000140221     C                   ENDdo
017100140221     C*
017200140221     C                   ENDSR
017300140221      * ?------------------------------------------------------------------ */
017400140221     C     CHK_segmento  BEGSR
017500140221     C*
017600140221     C*_______ UNA
017700140221     c                   If        tipo_segmento= 'UNA'
017800140225     c                   exsr      DATI_UNA
017900140221     C*_______ UNB
018000140221     c                   elseIf    tipo_segmento= 'UNB'
018100140225     c                   exsr      DATI_UNB
018200140221     C*_______
018300140221     c                   elseIf    tipo_segmento= 'UNH'
018400140225      * da ignorare
018500140221     C*_______
018600140221     c                   elseIf    tipo_segmento= 'BGM'
018700140225      * solo da salvare x utilizzare in seguito
018800140225     c                   eval       riga_BGM = FLATREC
018900140221     C*_______
019000140225      * inizio dettaglio
019100140221     c                   elseIf    tipo_segmento= 'CNI'
019200140225     c                   exsr      DATI_CNI
019300140221     C*_______
019400140221     c                   elseIf    tipo_segmento= 'STS'
019500140225      ****** al momento solo e sempre STS++21'
019600140225      *         quindi da ignorare
019700140225     c                   exsr      DATI_STS
019800140221     C*_______
019900140221     c                   elseIf    tipo_segmento= 'RFF'
020000140225     c                   exsr      DATI_RFF
020100140221     C*_______
020200140221     c                   elseIf    tipo_segmento= 'DTM'
020300140225     c                   exsr      DATI_DTM
020400140221     C*_______
020500140221     c                   elseIf    tipo_segmento= 'NAD'
020600140225     c                   exsr      DATI_NAD
020700140221     C*_______
020800140221     c                   elseIf    tipo_segmento= 'GID'
020900140225      *** al momento NO
021000140221     C*_______
021100140221     c                   elseIf    tipo_segmento= 'PCI'
021200140225      *** al momento NO
021300140221     C*_______
021400140221     c                   elseIf    tipo_segmento= 'TXT'
021500140225      *** al momento NO
021600140221     C*
021700140225     c                   elseIf    tipo_segmento= 'UNZ'
021800140225     c                   exsr      DATI_UNZ
021900140225     C*
022000140221     c                   end
022100140221     C*
022200140221     C                   ENDSR
022300140221      * ?------------------------------------------------------------------ */
022400140225     C     DATI_UNA      BEGSR
022500140225     C*
022600140225     c                   eval      campo_Dati = FLATREC
022700140225     c                   eval      campo_Dati = %Trim(campo_Dati)
022800140225     c                   exsr      WRI_TIVGD
022900140225     C*
023000140225     C                   ENDSR
023100140225      * ?------------------------------------------------------------------ */
023200140225     C     DATI_UNB      BEGSR
023300140225     C*
023400140225     c                   eval      campo_Dati = FLATREC
023500140225     c                   eval      campo_Dati = %Trim(campo_Dati)
023600140225     c                   exsr      WRI_TIVGD
023700140225     C*
023800140225      * Deve catturare il numero messaggio da riportare poi sull'ultima riga UNZ
023900140225     c     ' '           checkR(E) campo_Dati    position
024000140225     C                   SELECT
024100140225     C                   WHEN      %ERROR
024200140225     C*                               ... an error occurred
024300140225     C                   WHEN      %FOUND
024400140225     C*                               ... NUM is less than the full length of the string
024500140225     c                   eval      A = Position
024600140225     C                   ENDSL
024700140225      *
024800140225     c                   z-add     1             Num
024900140225     c                   z-add     1             Da
025000140225      *  cerca l'ultimo (+) iniziando da sinistra
025100140225     c                   DO        *hival
025200140225     c                   eval      position = %scan('+' : Campo_Dati : Num)
025300140225     c                   if        Position > 0
025400140225     c                   eval      Da  = position
025500140225     c                   eval      Num = position + 1
025600140225     c                   else
025700140225     c                   leave
025800140225     c                   end
025900140225     c                   endDO
026000140225      *
026100140225      * Salva per utilizzarlo in seguito sulla riga UNZ
026200140225     c                   eval      UNZ_sx = %subst(Campo_Dati: Da : 100)
026300140225     C*
026400140225     C                   ENDSR
026500140225      * ?------------------------------------------------------------------ */
026600140225     C     DATI_CNI      BEGSR
026700140225      *
026800140225     C* Solo dopo il primo  esegue chiusura del dettaglio precedente
026900140225     C                   if          Primo_CNI   = 'N'
027000140225     c                   exsr      Fine_Dett_prec
027100140225     c                   end
027200140225     C*
027300140225     C*  Inizio Dettaglio
027400140225     C*       contatore degli UNH
027500140225     c                   eval      UNH_cnt =  UNH_cnt  + 1
027600140225     C*   riga UNH
027700140225     c                   exsr      Conta_righe
027800140225     c                   eval      campo_Dati =
027900140225     c                             UNH_piu + %trim(%editc(UNH_cnt:'Z')) + UNH_dx
028000140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
028100140225     c                   exsr      WRI_TIVGD
028200140225     C*   riga BGM
028300140225     c                   exsr      Conta_righe
028400140225     c                   eval      campo_Dati = riga_BGM
028500140225     c                   exsr      WRI_TIVGD
028600140225     C*   riga NAD
028700140225     c                   exsr      Conta_righe
028800140225     c                   eval      campo_Dati = NAD_fw
028900140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
029000140225     c                   exsr      WRI_TIVGD
029100140225     C*   riga UNS+D
029200140225     c                   exsr      Conta_righe
029300140225     c                   eval      campo_Dati = UNS_d
029400140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
029500140225     c                   exsr      WRI_TIVGD
029600140225     C* --------->>>>
029700140225      *  Dopo il primo GIRO se lo ricorda
029800140225     C                   if          Primo_CNI   = 'S'
029900140225     C                   EVAL          Primo_CNI = 'N'
030000140225     c                   end
030100140225     C* --------->>>>
030200140225     C*
030300140225     C                   ENDSR
030400140225      * ?------------------------------------------------------------------ */
030500140225     C     Fine_Dett_precBEGSR
030600140225     C*
030700140225     C*   riga UNS+S
030800140225     c                   exsr      Conta_righe
030900140225     c                   eval      campo_Dati = UNS_s
031000140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
031100140225     c                   exsr      WRI_TIVGD
031200140225     C*
031300140225     C*   riga di chiusura UNT
031400140225     c                   exsr      Conta_righe
031500140225     c                   eval      campo_Dati =
031600140225     c                              UNT_piu + %trim(%editc(UNT_cnt:'Z')) + '+' +
031700140225     c                              %trim(%editc(UNH_cnt:'Z'))
031800140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
031900140225     c                   exsr      WRI_TIVGD
032000140225     C*
032100140225     C*   Azzera il conta righe
032200140225     c                   eval      UNT_cnt =  0
032300140225     C*
032400140225     C                   ENDSR
032500140225      * ?------------------------------------------------------------------ */
032600140225     C     DATI_STS      BEGSR
032700140225     C*
032800140225      *  se NON si sta mandando via solo i dati di CONSEGNA
032900140225     c                   if        %subst(FLATREC:1:7) <> 'STS++21'
033000140317      *----
033100140317     c                   if        %subst(FLATREC:1:7) =  'STS++23'
033200140317     c                   eval      salva_STS  = RSJ_fw + PRZ_consegnato
033300140317      *
033400140317     c                   elseif    %subst(FLATREC:1:11)=  'STS++14+117'
033500140317     c                   eval      salva_STS  = RSJ_fw + MANcante
033600140317      *
033700140317     c                   elseif    %subst(FLATREC:1:11)=  'STS++14+249'
033800140317     c                   eval      salva_STS  = RSJ_fw + RESO_autom
033900140317      *
034000140317     c                   else
034100140317      *
034200140317      *  se non previsto paro con XXX+XXX
034300140317     c                   eval      salva_STS  = RSJ_fw + NON_consegnato
034400140317     c                   end
034500140317      *----
034600140225     c                   else
034700140225      *
034800140225      *  COME da accordi solo CONSEGNATO!!!!!!
034900140225     c                   eval      salva_STS  = RSJ_fw + STS_consegnato
035000140225      *
035100140225     c                   end
035200140225     C*
035300140225     C                   ENDSR
035400140225      * ?------------------------------------------------------------------ */
035500140225     C     DATI_RFF      BEGSR
035600140225     C*
035700140225      *  Solo se è l'AGE
035800140225     c                   if        %subst(FLATREC:1:7) = 'RFF+AGE'
035900140225      **
036000140225      **  Imposta il Riferimento
036100140225     c                   exsr      Conta_righe
036200140225     c                   movel     RFF_SEN       FLATREC
036300140225     c                   eval      campo_Dati = FLATREC
036400140225     c                   exsr      WRI_TIVGD
036500140225     C*
036600140225     C* e in seguito lo stato di CONSEGNATO ....... CHIODATO
036700140225     c                   exsr      Conta_righe
036800140226      *                                      *------------------*
036900140226     c                   eval      campo_Dati =  salva_STS
037000140226      *                                      *------------------*
037100140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
037200140225     c                   exsr      WRI_TIVGD
037300140225     C*
037400140225     c                   end
037500140225     C*
037600140225     C                   ENDSR
037700140225      * ?------------------------------------------------------------------ */
037800140225     C     DATI_DTM      BEGSR
037900140225     C*
038000140225     c                   if        %subst(FLATREC:1:6) = 'DTM+7:'
038100140225     C*
038200140225      * Deve suddividere la data in 2 DATA e ORA
038300140225     c                   eval      DTM_data = %subst(FLATREC:9:6)
038400140225     c                   eval      DTM_ora  = %subst(FLATREC:15:4)
038500140317     c                   if        DTM_ora = ':102'
038600140317     c                   eval        DTM_ora = '0000'
038700140317     c                   end
038800140225      *
038900140225     c                   eval      campo_Dati =
039000140225     c                                DTM_DSJ + DTM_data + '+' + DTM_ora
039100140225     c                   exsr      Conta_righe
039200140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
039300140225     c                   exsr      WRI_TIVGD
039400140225     c                   end
039500140225     C*
039600140225     C                   ENDSR
039700140225      * ?------------------------------------------------------------------ */
039800140225     C     DATI_NAD      BEGSR
039900140225     C*
040000140225      *  Solo se c'è
040100140225     c                   if        %subst(FLATREC:1:6) = 'NAD+AP'
040200140225     c                   exsr      Conta_righe
040300140225     c                   movel     TXT_DEL       FLATREC
040400140225     c                   eval      campo_Dati = FLATREC
040500140225     c                   exsr      WRI_TIVGD
040600140225     c                   end
040700140225     C*
040800140225     C                   ENDSR
040900140225      * ?------------------------------------------------------------------ */
041000140225     C     DATI_UNZ      BEGSR
041100140225     C*
041200140225     C*  Chiude l'ultimo dettaglio
041300140225     c                   exsr      Fine_Dett_prec
041400140225     C*
041500140225     c                   eval      campo_Dati =
041600140225     c                              UNZ_piu + %trim(%editc(UNH_cnt:'Z'))
041700140225     c                              +  UNZ_sx
041800140225     c                   exsr      WRI_TIVGD
041900140225     C*
042000140225      * quindi chiude
042100140225      *   il TIVGD
042200140225     C                   EXSR      vgd_CLOSE
042300140225      *
042400140225     C                   ENDSR
042500140225      * ?------------------------------------------------------------------ */
042600140225     C     Conta_righe   BEGSR
042700140225     C*
042800140225     c                   eval      UNT_cnt =  UNT_cnt  + 1
042900140225     C*
043000140225     C                   ENDSR
043100140225      **?__________________________________________________________________ */
043200140225      *    Apertura del TIVGD
043300140225      **?__________________________________________________________________ */
043400140225     C     SOLO_UNA_VOLTABEGSR
043500140225     C*
043600140225      * BASTA la PRIMA RIGA LETTA del WORKFILE
043700140225     c                   if        in_generale = *blank
043800140225     c                   eval         in_generale = 'S'
043900140225      *
044000140225      *  recupera il progressivo
044100140225     c                   exsr      Piglia_progr
044200140225      *
044300140225      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
044400140225     C                   clear                   trul47ds
044500140225     C                   eval      d47opz  = 'I'
044600140225     C                   eval      d47tip  = TIPOF
044700140225     C                   eval      d47lck  = 'N'
044800140225     C                   eval      d47chkj = 'N'
044900140225     C                   eval      d47pgm  = 'TRTCT95R'
045000140225     C                   call      'TRUL47R'
045100140225     C                   parm                    trul47ds
045200140225     C*
045300140225     c                   end
045400140225     C*
045500140225     C                   ENDSR
045600140225      **?__________________________________________________________________ */
045700140225      *   Prende il Progressivo
045800140225      **?__________________________________________________________________ */
045900140225     C     Piglia_progr  Begsr
046000140225      *
046100140225     C                   MOVEL     w95KSU        virKSC
046200140225     C                   MOVEL     TIPOF         virTIP
046300140225     c     kvir02        chain     tivir02l
046400140225     c                   move      'OF'          Var_ISV
046500140225     c                   if        %Found(tivir02l)
046600140225     c                   move      VirFI1        Var_ISV
046700140225     C                   End
046800140225      *
046900140225      *       prende numeratore strategi
047000140225     C                   exsr      calprog
047100140225      *
047200140225     c                   endsr
047300140225      **?__________________________________________________________________ */
047400140225     C     calprog       begsr
047500140225     C*
047600140225     C                   open      tis7prgf
047700140225     C                   read(e)   tis7prgf
047800140225     C*
047900140225     C                   if        not %error
048000140225     C                   eval      dwlprg = f_tis7prgf
048100140225     C*
048200140225     C                   move(p)   dwlprg        wrkprg
048300140225     C                   add       1             wrkprg
048400140225     C                   move(p)   wrkprg        dwlprg
048500140225     C                   movel     Var_ISV       dwlprg
048600140225     C*
048700140225     C                   eval       f_tis7prgf = dwlprg
048800140225     C                   eval      Progressivo = dwlprg
048900140225     C*
049000140225     C                   update    tis7prg0
049100140225     C                   endif
049200140225     C*
049300140225     C                   close     tis7prgf
049400140225     C*
049500140225     C                   endsr
049600140225      **?__________________________________________________________________ */
049700140225     C     WRI_TIVGD     BEGSR
049800140225     C*
049900140225     c                   clear                   tivgd000
050000140225     c                   eval      vgddta = %TrimR(campo_Dati)
050100140225     c                   eval      vgdtip = TipoF
050200140225     c                   eval      vgdksu = w95KSU
050300140225     C                   eval      vgdprg = Progressivo
050400140225     c                   eval      vgdtsc = 'WW'
050500140225     c                   eval      vgdpgm = 'TRTCT95R'
050600140225     c                   eval      vgddat = udtymd
050700140225     c                   write     tivgd000
050800140225     C*
050900140221     C                   ENDSR
051000140225      **?__________________________________________________________________ */
051100140225      *?      X non bloccare in nessun caso il traduttore CLIENTI
051200140225      **?__________________________________________________________________ */
051300140225     C     *pssr         BEGSR
051400140225     C
051500140225     C                   eval      prmesito = '2'
051600140225     c                   exsr      invio_mail
051700140225     C                   ENDSR     '*CANCL'
051800140225     C
051900140225     c*==================================================================*
052000140225      * Manda un Msg x E-mail
052100140225     c*==================================================================*
052200140225     c     Invio_Mail    begsr
052300140225      *
052400140225     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
052500140225     C* Inizializzo variabili
052600140225     C                   movel     *blanks       wrkEml          253
052700140225     C                   movel     *blanks       wrkMsg         5000
052800140225     C                   movel     *blanks       wrkOgg           44
052900140225     C* Valorizzo i campi della e-m@ail - indirizzo
053000140225     C                   eval      wrkEml = CED_BART
053100140225     C                   eval      wrkOgg ='Problemi DOWNLD HAGER EDI !!'
053200140225     C                   EVAL      wrkMSG = 'ATTENZIONE : traduzione EDI -
053300140225     C                             STATI << HAGER >> errato su DOWNLD. -
053400140225     C                             Controllare il Download del Cliente. >> '
053500140225     C                   call(e)   'TRTCT00R2'
053600140225     C                   parm                    wrkEml
053700140225     C                   parm                    wrkOgg
053800140225     C                   parm                    wrkMsg
053900140225     C*
054000140225     C                   ENDSR
054100140225      **?__________________________________________________________________ */
054200140225      *    Chiusura del TIVGD
054300140225      **?__________________________________________________________________ */
054400140225     C     VGD_CLOSE     Begsr
054500140225      *
054600140225     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
054700140225     C                   clear                   trul47ds
054800140225     C                   eval      d47opz  = 'F'
054900140225     C                   eval      d47tip  = TipoF
055000140225     C                   call      'TRUL47R'
055100140225     C                   parm                    trul47ds
055200140225      *
055300140225     c                   endsr
055400140225      * ?------------------------------------------------------------------ */
