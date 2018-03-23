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
001500110519      **?__________________________________________________________________ */
001600140220      *                                                                     *
001700140220      * Scrittura del REPORT:2:3:GT:GTF   x CEVA/HAGER (IFTSTA)             *
001800140221      *                                                                     *
001900140221      *   così formattato:                                                  *
002000140221      *            UNA:+.? '                                                *
002100140221      *            UNB+UNOA:2+ITBAR005:ZZ+HAGER:22+140219:2003+2889'        *
002200140221      *            UNH+1+REPORT:2:3:GT:GTF'                                 *
002300140221      *            BGM+784+20030419022014+9'                                *
002400140221      *            NAD+FW+04507990150+BRT SPA?                              *
002500140221      *            UNS+D'                                                   *
002600140221      *            RFF+SEN+21400142'                                        *
002700140221      *            RSJ+FW+LIV+CFM'                                          *
002800140225      *            DTM+DSJ+140113+1045'                                     *
002900140221      *            TXT+DEL+++NICOLA'                                        *
003000140221      *            UNS+S'                                                   *
003100140221      *            UNT+10+1'                                                *
003200140221      *            UNH+2+REPORT:2:3:GT:GTF'                                 *
003300140221      *            BGM+784+20030419022014+9'                                *
003400140221      *            NAD+FW+04507990150+BRT SPA?                              *
003500140221      *            UNS+D'                                                   *
003600140221      *            RFF+SEN+21400148'                                        *
003700140221      *            RSJ+FW+LIV+CFM'                                          *
003800140225      *            DTM+DSJ+140113+1045'                                     *
003900140221      *            TXT+DEL+++NICOLA'                                        *
004000140221      *            UNS+S'                                                   *
004100140221      *            UNT+10+2'                                                *
004200140221      *            UNH+3+REPORT:2:3:GT:GTF'                                 *
004300140221      *            BGM+784+20030419022014+9'                                *
004400140221      *            NAD+FW+04507990150+BRT SPA?                              *
004500140221      *            UNS+D'                                                   *
004600140221      *            RFF+SEN+21400223'                                        *
004700140221      *            RSJ+FW+LIV+CFM'                                          *
004800140221      *            DTM+DSJ+140114+1200'                                     *
004900140221      *            UNS+S'                                                   *
005000140221      *            UNT+9+3'                                                 *
005100140221      *            UNZ+3+2889'                                              *
005200140221      *                                                                     *
005300050927      **?__________________________________________________________________ */
005400140220     FTRTCT95W  IF   E           K DISK
005500110519      *
005600140225     ftivgd00f  o  a E             DISK    COMMIT
005700140225      *
005800140225     Ftivir02L  IF   E           K DISK
005900140225     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
006000140225     F                                     PREFIX(f_) usropn
006100110519      **?__________________________________________________________________ */
006200140221     D prmppt          s             50
006300140221     D prmesito        s              1
006400140221     D wrkesito        s                   like(prmesito)
006500140225      **?__________________________________________________________________ */
006600140225     D TipoF           C                   CONST('EC')
006700140225     D dwlprg          s             10    INZ(*all'0')
006800140225     D wrkprg          s              8  0 INZ(*zeros)
006900140225     D Var_ISV         s                   like(Virfi1)
007000140225     d trul47ds      e ds
007100140225      **?__________________________________________________________________ */
007200140225     D progressivo     s             10    inz(' ')
007300140225     D in_generale     s              1    inz(' ')
007400140225      **
007500140225     D Tipo_segmento   s              3
007600140225     D Primo_CNI       s              1
007700130502      **
007800140221     D campo_dati      s                   like(FLATREC)
007900140221      **
008000140225     D UNH_piu         s              4    INZ('UNH+')
008100140225     D UNH_cnt         s              5s 0 INZ(0)
008200140221     D UNH_dx          s             18    INZ('+REPORT:2:3:GT:GTF')
008300140221      **
008400140225     D riga_BGM        s                   like(FLATREC)
008500140225     D salva_STS       s                   like(FLATREC)
008600140221      **
008700140221     D NAD_fw          s             26    INZ('NAD+FW+04507990150+BRT SPA')
008800140225      **
008900140221     D UNS_d           s              5    INZ('UNS+D')
009000140221      **
009100140221     D RFF_SEN         s              8    INZ('RFF+SEN+')
009200140221      **
009300140221     D RSJ_fw          s              7    INZ('RSJ+FW+')
009400140221     D STS_consegnato  s              7    INZ('LIV+CFM')
009500140225     D NON_consegnato  s              7    INZ('XXX+XXX')
009600140225      **
009700140225     D DTM_DSJ         s              8    INZ('DTM+DSJ+')
009800140225     D DTM_data        s              6    INZ(' ')
009900140225     D DTM_ora         s              4    INZ(' ')
010000140225      **
010100140225     D TXT_DEL         s              7    INZ('TXT+DEL')
010200140221      **
010300140221     D UNS_s           s              5    INZ('UNS+S')
010400140221      **
010500140225     D UNT_piu         s              4    INZ('UNT+')
010600140225     D UNT_cnt         s              5s 0 INZ(0)
010700140221      **
010800140225     D UNZ_piu         s              4    INZ('UNZ+')
010900140221      **
011000140221     D UNZ_sx          s            100    INZ(' ')
011100140221      **
011200140221     D Position        s              5s 0 INZ(0)
011300140221     D num             s              5s 0 INZ(0)
011400140221     D Da              s              5s 0 INZ(0)
011500140221     D A               s              5s 0 INZ(0)
011600140225
011700140225     d CED_Bart        C                   CONST('CEDAlert@Brt.it;')
011800050927      **?__________________________________________________________________ */
011900050704      *    main
012000110316      **?__________________________________________________________________ */
012100130415     c     *entry        plist
012200140221     C                   parm                    prmppt
012300140221     C     wrkesito      parm      wrkesito      prmesito
012400140225     c*
012500140225     c     kvir02        klist
012600140225     C                   kfld                    virKSC
012700140225     C                   kfld                    virTIP
012800140221      *
012900140221     C                   eval      wrkesito = '0'
013000140225     c*
013100140225     C                   TIME                    ORADAT           14 0
013200140225     C                   MOVEL     ORADAT        ORATR             6 0
013300140225     c                   move      *date         udtymd            8 0
013400140225      * ------
013500140225     C                   exsr      CONVERTE
013600140225      *
013700140225     c                   COMMIT
013800140221      *   chiude in LR
013900140221     c                   seton                                        LR
014000140225      **?__________________________________________________________________ */
014100140225     C     CONVERTE      BEGSR
014200140221     C*
014300140221     C*    per capire solo quando si troverà a scrivere il primo CNI
014400140221     C                   EVAL      Primo_CNI = 'S'
014500140221     C*
014600140221     C*   legge il flat di work e rilascia i records scrivendoli sul VGD
014700140221     C*    trasformati
014800140225     C                   read      TRTCT95W
014900140221     C                   dow       not %eof(TRTCT95W)
015000140225     C*  decod.generali
015100140225     C                   exsr      SOLO_UNA_VOLTA
015200140225     C*
015300140221     c                   eval      tipo_segmento = %subst(flatrec:1:3)
015400140221     c                   exsr      CHK_segmento
015500140221     C*
015600140221     C                   read      TRTCT95W
015700140221     C                   ENDdo
015800140221     C*
015900140221     C                   ENDSR
016000140221      * ?------------------------------------------------------------------ */
016100140221     C     CHK_segmento  BEGSR
016200140221     C*
016300140221     C*_______ UNA
016400140221     c                   If        tipo_segmento= 'UNA'
016500140225     c                   exsr      DATI_UNA
016600140221     C*_______ UNB
016700140221     c                   elseIf    tipo_segmento= 'UNB'
016800140225     c                   exsr      DATI_UNB
016900140221     C*_______
017000140221     c                   elseIf    tipo_segmento= 'UNH'
017100140225      * da ignorare
017200140221     C*_______
017300140221     c                   elseIf    tipo_segmento= 'BGM'
017400140225      * solo da salvare x utilizzare in seguito
017500140225     c                   eval       riga_BGM = FLATREC
017600140221     C*_______
017700140225      * inizio dettaglio
017800140221     c                   elseIf    tipo_segmento= 'CNI'
017900140225     c                   exsr      DATI_CNI
018000140221     C*_______
018100140221     c                   elseIf    tipo_segmento= 'STS'
018200140225      ****** al momento solo e sempre STS++21'
018300140225      *         quindi da ignorare
018400140225     c                   exsr      DATI_STS
018500140221     C*_______
018600140221     c                   elseIf    tipo_segmento= 'RFF'
018700140225     c                   exsr      DATI_RFF
018800140221     C*_______
018900140221     c                   elseIf    tipo_segmento= 'DTM'
019000140225     c                   exsr      DATI_DTM
019100140221     C*_______
019200140221     c                   elseIf    tipo_segmento= 'NAD'
019300140225     c                   exsr      DATI_NAD
019400140221     C*_______
019500140221     c                   elseIf    tipo_segmento= 'GID'
019600140225      *** al momento NO
019700140221     C*_______
019800140221     c                   elseIf    tipo_segmento= 'PCI'
019900140225      *** al momento NO
020000140221     C*_______
020100140221     c                   elseIf    tipo_segmento= 'TXT'
020200140225      *** al momento NO
020300140221     C*
020400140225     c                   elseIf    tipo_segmento= 'UNZ'
020500140225     c                   exsr      DATI_UNZ
020600140225     C*
020700140221     c                   end
020800140221     C*
020900140221     C                   ENDSR
021000140221      * ?------------------------------------------------------------------ */
021100140225     C     DATI_UNA      BEGSR
021200140225     C*
021300140225     c                   eval      campo_Dati = FLATREC
021400140225     c                   eval      campo_Dati = %Trim(campo_Dati)
021500140225     c                   exsr      WRI_TIVGD
021600140225     C*
021700140225     C                   ENDSR
021800140225      * ?------------------------------------------------------------------ */
021900140225     C     DATI_UNB      BEGSR
022000140225     C*
022100140225     c                   eval      campo_Dati = FLATREC
022200140225     c                   eval      campo_Dati = %Trim(campo_Dati)
022300140225     c                   exsr      WRI_TIVGD
022400140225     C*
022500140225      * Deve catturare il numero messaggio da riportare poi sull'ultima riga UNZ
022600140225     c     ' '           checkR(E) campo_Dati    position
022700140225     C                   SELECT
022800140225     C                   WHEN      %ERROR
022900140225     C*                               ... an error occurred
023000140225     C                   WHEN      %FOUND
023100140225     C*                               ... NUM is less than the full length of the string
023200140225     c                   eval      A = Position
023300140225     C                   ENDSL
023400140225      *
023500140225     c                   z-add     1             Num
023600140225     c                   z-add     1             Da
023700140225      *  cerca l'ultimo (+) iniziando da sinistra
023800140225     c                   DO        *hival
023900140225     c                   eval      position = %scan('+' : Campo_Dati : Num)
024000140225     c                   if        Position > 0
024100140225     c                   eval      Da  = position
024200140225     c                   eval      Num = position + 1
024300140225     c                   else
024400140225     c                   leave
024500140225     c                   end
024600140225     c                   endDO
024700140225      *
024800140225      * Salva per utilizzarlo in seguito sulla riga UNZ
024900140225     c                   eval      UNZ_sx = %subst(Campo_Dati: Da : 100)
025000140225     C*
025100140225     C                   ENDSR
025200140225      * ?------------------------------------------------------------------ */
025300140225     C     DATI_CNI      BEGSR
025400140225      *
025500140225     C* Solo dopo il primo  esegue chiusura del dettaglio precedente
025600140225     C                   if          Primo_CNI   = 'N'
025700140225     c                   exsr      Fine_Dett_prec
025800140225     c                   end
025900140225     C*
026000140225     C*  Inizio Dettaglio
026100140225     C*       contatore degli UNH
026200140225     c                   eval      UNH_cnt =  UNH_cnt  + 1
026300140225     C*   riga UNH
026400140225     c                   exsr      Conta_righe
026500140225     c                   eval      campo_Dati =
026600140225     c                             UNH_piu + %trim(%editc(UNH_cnt:'Z')) + UNH_dx
026700140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
026800140225     c                   exsr      WRI_TIVGD
026900140225     C*   riga BGM
027000140225     c                   exsr      Conta_righe
027100140225     c                   eval      campo_Dati = riga_BGM
027200140225     c                   exsr      WRI_TIVGD
027300140225     C*   riga NAD
027400140225     c                   exsr      Conta_righe
027500140225     c                   eval      campo_Dati = NAD_fw
027600140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
027700140225     c                   exsr      WRI_TIVGD
027800140225     C*   riga UNS+D
027900140225     c                   exsr      Conta_righe
028000140225     c                   eval      campo_Dati = UNS_d
028100140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
028200140225     c                   exsr      WRI_TIVGD
028300140225     C* --------->>>>
028400140225      *  Dopo il primo GIRO se lo ricorda
028500140225     C                   if          Primo_CNI   = 'S'
028600140225     C                   EVAL          Primo_CNI = 'N'
028700140225     c                   end
028800140225     C* --------->>>>
028900140225     C*
029000140225     C                   ENDSR
029100140225      * ?------------------------------------------------------------------ */
029200140225     C     Fine_Dett_precBEGSR
029300140225     C*
029400140225     C*   riga UNS+S
029500140225     c                   exsr      Conta_righe
029600140225     c                   eval      campo_Dati = UNS_s
029700140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
029800140225     c                   exsr      WRI_TIVGD
029900140225     C*
030000140225     C*   riga di chiusura UNT
030100140225     c                   exsr      Conta_righe
030200140225     c                   eval      campo_Dati =
030300140225     c                              UNT_piu + %trim(%editc(UNT_cnt:'Z')) + '+' +
030400140225     c                              %trim(%editc(UNH_cnt:'Z'))
030500140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
030600140225     c                   exsr      WRI_TIVGD
030700140225     C*
030800140225     C*   Azzera il conta righe
030900140225     c                   eval      UNT_cnt =  0
031000140225     C*
031100140225     C                   ENDSR
031200140225      * ?------------------------------------------------------------------ */
031300140225     C     DATI_STS      BEGSR
031400140225     C*
031500140225      *  se NON si sta mandando via solo i dati di CONSEGNA
031600140225     c                   if        %subst(FLATREC:1:7) <> 'STS++21'
031700140225      *
031800140225     c                   eval      salva_STS  = RSJ_fw + NON_consegnato
031900140225      *
032000140225     c                   else
032100140225      *
032200140225      *  COME da accordi solo CONSEGNATO!!!!!!
032300140225     c                   eval      salva_STS  = RSJ_fw + STS_consegnato
032400140225      *
032500140225     c                   end
032600140225     C*
032700140225     C                   ENDSR
032800140225      * ?------------------------------------------------------------------ */
032900140225     C     DATI_RFF      BEGSR
033000140225     C*
033100140225      *  Solo se è l'AGE
033200140225     c                   if        %subst(FLATREC:1:7) = 'RFF+AGE'
033300140225      **
033400140225      **  Imposta il Riferimento
033500140225     c                   exsr      Conta_righe
033600140225     c                   movel     RFF_SEN       FLATREC
033700140225     c                   eval      campo_Dati = FLATREC
033800140225     c                   exsr      WRI_TIVGD
033900140225     C*
034000140225     C* e in seguito lo stato di CONSEGNATO ....... CHIODATO
034100140225     c                   exsr      Conta_righe
034200140226      *                                      *------------------*
034300140226     c                   eval      campo_Dati =  salva_STS
034400140226      *                                      *------------------*
034500140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
034600140225     c                   exsr      WRI_TIVGD
034700140225     C*
034800140225     c                   end
034900140225     C*
035000140225     C                   ENDSR
035100140225      * ?------------------------------------------------------------------ */
035200140225     C     DATI_DTM      BEGSR
035300140225     C*
035400140225     c                   if        %subst(FLATREC:1:6) = 'DTM+7:'
035500140225     C*
035600140225      * Deve suddividere la data in 2 DATA e ORA
035700140225     c                   eval      DTM_data = %subst(FLATREC:9:6)
035800140225     c                   eval      DTM_ora  = %subst(FLATREC:15:4)
035900140225      *
036000140225     c                   eval      campo_Dati =
036100140225     c                                DTM_DSJ + DTM_data + '+' + DTM_ora
036200140225     c                   exsr      Conta_righe
036300140225     c                   eval      campo_Dati = %Trim(campo_Dati) + ''''
036400140225     c                   exsr      WRI_TIVGD
036500140225     c                   end
036600140225     C*
036700140225     C                   ENDSR
036800140225      * ?------------------------------------------------------------------ */
036900140225     C     DATI_NAD      BEGSR
037000140225     C*
037100140225      *  Solo se c'è
037200140225     c                   if        %subst(FLATREC:1:6) = 'NAD+AP'
037300140225     c                   exsr      Conta_righe
037400140225     c                   movel     TXT_DEL       FLATREC
037500140225     c                   eval      campo_Dati = FLATREC
037600140225     c                   exsr      WRI_TIVGD
037700140225     c                   end
037800140225     C*
037900140225     C                   ENDSR
038000140225      * ?------------------------------------------------------------------ */
038100140225     C     DATI_UNZ      BEGSR
038200140225     C*
038300140225     C*  Chiude l'ultimo dettaglio
038400140225     c                   exsr      Fine_Dett_prec
038500140225     C*
038600140225     c                   eval      campo_Dati =
038700140225     c                              UNZ_piu + %trim(%editc(UNH_cnt:'Z'))
038800140225     c                              +  UNZ_sx
038900140225     c                   exsr      WRI_TIVGD
039000140225     C*
039100140225      * quindi chiude
039200140225      *   il TIVGD
039300140225     C                   EXSR      vgd_CLOSE
039400140225      *
039500140225     C                   ENDSR
039600140225      * ?------------------------------------------------------------------ */
039700140225     C     Conta_righe   BEGSR
039800140225     C*
039900140225     c                   eval      UNT_cnt =  UNT_cnt  + 1
040000140225     C*
040100140225     C                   ENDSR
040200140225      **?__________________________________________________________________ */
040300140225      *    Apertura del TIVGD
040400140225      **?__________________________________________________________________ */
040500140225     C     SOLO_UNA_VOLTABEGSR
040600140225     C*
040700140225      * BASTA la PRIMA RIGA LETTA del WORKFILE
040800140225     c                   if        in_generale = *blank
040900140225     c                   eval         in_generale = 'S'
041000140225      *
041100140225      *  recupera il progressivo
041200140225     c                   exsr      Piglia_progr
041300140225      *
041400140225      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
041500140225     C                   clear                   trul47ds
041600140225     C                   eval      d47opz  = 'I'
041700140225     C                   eval      d47tip  = TIPOF
041800140225     C                   eval      d47lck  = 'N'
041900140225     C                   eval      d47chkj = 'N'
042000140225     C                   eval      d47pgm  = 'TRTCT95R'
042100140225     C                   call      'TRUL47R'
042200140225     C                   parm                    trul47ds
042300140225     C*
042400140225     c                   end
042500140225     C*
042600140225     C                   ENDSR
042700140225      **?__________________________________________________________________ */
042800140225      *   Prende il Progressivo
042900140225      **?__________________________________________________________________ */
043000140225     C     Piglia_progr  Begsr
043100140225      *
043200140225     C                   MOVEL     w95KSU        virKSC
043300140225     C                   MOVEL     TIPOF         virTIP
043400140225     c     kvir02        chain     tivir02l
043500140225     c                   move      'OF'          Var_ISV
043600140225     c                   if        %Found(tivir02l)
043700140225     c                   move      VirFI1        Var_ISV
043800140225     C                   End
043900140225      *
044000140225      *       prende numeratore strategi
044100140225     C                   exsr      calprog
044200140225      *
044300140225     c                   endsr
044400140225      **?__________________________________________________________________ */
044500140225     C     calprog       begsr
044600140225     C*
044700140225     C                   open      tis7prgf
044800140225     C                   read(e)   tis7prgf
044900140225     C*
045000140225     C                   if        not %error
045100140225     C                   eval      dwlprg = f_tis7prgf
045200140225     C*
045300140225     C                   move(p)   dwlprg        wrkprg
045400140225     C                   add       1             wrkprg
045500140225     C                   move(p)   wrkprg        dwlprg
045600140225     C                   movel     Var_ISV       dwlprg
045700140225     C*
045800140225     C                   eval       f_tis7prgf = dwlprg
045900140225     C                   eval      Progressivo = dwlprg
046000140225     C*
046100140225     C                   update    tis7prg0
046200140225     C                   endif
046300140225     C*
046400140225     C                   close     tis7prgf
046500140225     C*
046600140225     C                   endsr
046700140225      **?__________________________________________________________________ */
046800140225     C     WRI_TIVGD     BEGSR
046900140225     C*
047000140225     c                   clear                   tivgd000
047100140225     c                   eval      vgddta = %TrimR(campo_Dati)
047200140225     c                   eval      vgdtip = TipoF
047300140225     c                   eval      vgdksu = w95KSU
047400140225     C                   eval      vgdprg = Progressivo
047500140225     c                   eval      vgdtsc = 'WW'
047600140225     c                   eval      vgdpgm = 'TRTCT95R'
047700140225     c                   eval      vgddat = udtymd
047800140225     c                   write     tivgd000
047900140225     C*
048000140221     C                   ENDSR
048100140225      **?__________________________________________________________________ */
048200140225      *?      X non bloccare in nessun caso il traduttore CLIENTI
048300140225      **?__________________________________________________________________ */
048400140225     C     *pssr         BEGSR
048500140225     C
048600140225     C                   eval      prmesito = '2'
048700140225     c                   exsr      invio_mail
048800140225     C                   ENDSR     '*CANCL'
048900140225     C
049000140225     c*==================================================================*
049100140225      * Manda un Msg x E-mail
049200140225     c*==================================================================*
049300140225     c     Invio_Mail    begsr
049400140225      *
049500140225     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
049600140225     C* Inizializzo variabili
049700140225     C                   movel     *blanks       wrkEml          253
049800140225     C                   movel     *blanks       wrkMsg         5000
049900140225     C                   movel     *blanks       wrkOgg           44
050000140225     C* Valorizzo i campi della e-m@ail - indirizzo
050100140225     C                   eval      wrkEml = CED_BART
050200140225     C                   eval      wrkOgg ='Problemi DOWNLD HAGER EDI !!'
050300140225     C                   EVAL      wrkMSG = 'ATTENZIONE : traduzione EDI -
050400140225     C                             STATI << HAGER >> errato su DOWNLD. -
050500140225     C                             Controllare il Download del Cliente. >> '
050600140225     C                   call(e)   'TRTCT00R2'
050700140225     C                   parm                    wrkEml
050800140225     C                   parm                    wrkOgg
050900140225     C                   parm                    wrkMsg
051000140225     C*
051100140225     C                   ENDSR
051200140225      **?__________________________________________________________________ */
051300140225      *    Chiusura del TIVGD
051400140225      **?__________________________________________________________________ */
051500140225     C     VGD_CLOSE     Begsr
051600140225      *
051700140225     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
051800140225     C                   clear                   trul47ds
051900140225     C                   eval      d47opz  = 'F'
052000140225     C                   eval      d47tip  = TipoF
052100140225     C                   call      'TRUL47R'
052200140225     C                   parm                    trul47ds
052300140225      *
052400140225     c                   endsr
052500140225      * ?------------------------------------------------------------------ */
