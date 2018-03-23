000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200050707     H DECEDIT('0.') DATEDIT(*YMD.)
000300130429     H* TRTCT97R         ---------------------------------------------------*
000400110519      **?__________________________________________________________________ */
000500110519     H*                                                                     *
000600130429     H* Scrittura x GEL degli O.R.M.   Partner  (EXPORT)                    *
000700130429     H*  versione GEL                                                       *
000800050927      **?__________________________________________________________________ */
000900130415     FfnORM01L  IF   E           K DISK
001000130418     FedTAB01L  IF   E           K DISK
001100110519      *
001200130429     ftivgd00f  if a E             DISK    commit
001300110617     Ftivir02L  IF   E           K DISK
001400110617     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001500110617     F                                     PREFIX(f_) usropn
001600130429      **?************************************************************************
001700130520???? D trtctORMds    E DS                  prefix(T97:3)
001800130419      **?************************************************************************
001900130415     D EDIDSPT       E DS
002000130415     D EDIDSPU       E DS
002100130415     D EDIDSPV       E DS
002200130415     D EDIDSPZ       E DS
002300130419     D EDIDSPO       E DS
002400130415      **?************************************************************************
002500130415     D KPJBA         E DS
002600130415     D     svkpjbu     s                   like(kpjbu)
002700130415      **?************************************************************************
002800130415      **  Definizione Qualificatori
002900130415      **?************************************************************************
003000130429     d GEL_DE          C                   CONST('GEL_DE')
003100130415      *
003200130415     D TRUL0SDS      E DS
003300130415      *
003400130415      * Tabella partner
003500130415     D PT_key          S             35    DIM(200)
003600130415     D PT_uni          S             90    DIM(%elem(PT_key))
003700130415     D PU_uni          S             90    DIM(%elem(PT_key))
003800130415     D PV_uni          S             90    DIM(%elem(PT_key))
003900130415     D PZ_uni          S             90    DIM(%elem(PT_key))
004000130419     D PO_uni          S             90    DIM(%elem(PT_key))
004100130415      *
004200130415     D PU_key          S             35
004300130415     D PV_key          S             35
004400130415     D PZ_key          S             35
004500130419     D PO_key          S             35
004600130429      *
004700130430     D SAV_Progr       S                   like(vgdPRG)
004800130415      * ?================================================================== */
004900130416      *  Invio E-mail
005000130416     D Indirizzi       s            253
005100130416     D Oggetto         s             44
005200130416     D Messaggio       s           5000
005300130416     d   DSmail        ds
005400130416     D Mail_Ind                      44
005500130416     d   IND_char                     1    dim(44)
005600130416      *
005700130416     D Lunghezza_Indirizzo...
005800130416     D                 s              5u 0
005900130416     D dalCarattere    s              5u 0
006000130416     D xTOTcaratteri   s              5u 0
006100130416      *
006200130416     d bart_it         C                   CONST('@Brt.it;')
006300130416     d CED_Bart        C                   CONST('CED@Brt.it;')
006400130415      *---------------------------------------------------------------------*
006500130429     D TipoF           C                   CONST('EM')
006600110617     D dwlprg          s             10    INZ(*all'0')
006700110617     D wrkprg          s              8  0 INZ(*zeros)
006800060814     d trul47ds      e ds
006900060814     D W0140           S             14  0 inz
007000110316     D Segmento        s           2048
007100110316     D Conta_righe     s              5i 0
007200110617      *---------------------------------------------------------------------*
007300110617     D Var_ISV         s                   like(Virfi1)
007400050927      **?__________________________________________________________________ */
007500050704      *    main
007600110316      **?__________________________________________________________________ */
007700130415     c     *entry        plist
007800130415     C                   PARM                    kpjba
007900130520     C                   movel     kpjbu         trtctORMds
008000130416     c*
008100130430     c*  DEVE essere richiamato correttamente con la FUNZIONE apposita
008200130429     c                   clear                   T97ESITO          1
008300130430     c                   IF        T97FUNZION <>'OPEN ' and
008400130430     c                             T97FUNZION <>'EXEC ' and
008500130430     c                             T97FUNZION <>'CLOSE'
008600130430     c                   move      '1'           T97ESITO
008700130430???? C                   EVAL      Indirizzi = CED_Bart
008800130430???? C                   EVAL      Oggetto ='Problemi ORM DOWNLOAD x GEL'
008900130430     C                   EVAL      Messaggio = 'ATTENZIONE : -
009000130430     C                             il TRTCT97R è stato richiamato in MODO -
009100130430     C                             anomalo. NON è stata passata la corrett-
009200130430     C                             a modalità: OPEN/EXEC/CLOSE. Controllar-
009300130430     c                             e il programma Chiamante e corregerlo!'
009400130430     c                   exsr      Invio_Mail
009500130430     c                   SETON                                        LR
009600130430     c                   RETURN
009700130430     c                   end
009800130429      *
009900130416     c     kediTAB       klist
010000130416     C                   kfld                    tabCOD
010100130416     C                   kfld                    tabKEY
010200130415     c*
010300130415     c     kORM          klist
010400130429     C                   kfld                    T97ORMPOE
010500130429     C                   kfld                    T97ORMNSR
010600130429     C                   kfld                    T97ORMNOR
010700130429     C                   kfld                    T97ORMNRV
010800130415     c*
010900110617     c     kvir02        klist
011000110617     C                   kfld                    virKSC
011100110617     C                   kfld                    virTIP
011200130415      *
011300130429      *  Se deve INIZIARE passa l' OPEN
011400130429      * ?              /*--------------------------- */
011500130429     c                   IF        T97FUNZION = 'OPEN '
011600130429     c                               or
011700130429     C                             SAV_Progr   = *blank
011800130429      * ?              /*--------------------------- */
011900130429      *
012000130429     C                   TIME                    ORADAT           14 0
012100130429     C                   MOVEL     ORADAT        ORATR             6 0
012200130429     c                   move      *date         udtymd            8 0
012300130429     C                   TIME                    W0140
012400130429     C                   MOVE      W0140         W0080             8 0
012500130429     C                   MOVE      T97CLIENTE    Cliente7n         7 0
012600130430     C                   MOVE      *zeros        CLIENTE8A         8
012700130430     C                   MOVE      T97CLIENTE    CLIENTE8A
012800130521     c                   move      *blanks       CLIENTE_trs       8
012900130521      *
013000130521     c                   EXSR      vgd_OPEN
013100130521      * ?              /*--------------------------- */
013200130429     c                   exsr      CARICA_EDTAB
013300130521      * ?              /*--------------------------- */
013400130429      **
013500130430      * quindi chiude e conferma la scrittura del TIVGD
013600130429      * ?              /*--------------------------- */
013700130429     c                   elseIF    T97FUNZION = 'CLOSE'
013800130429      * ?              /*--------------------------- */
013900130429     c                   commit
014000130429     C                   EXSR      vgd_CLOSE
014100130429     c                   seton                                        LR
014200130429     c                   RETURN
014300130429      **
014400130429      * ?              /*--------------------------- */
014500130429     c                   end
014600130429      * ?              /*--------------------------- */
014700130429      **
014800130430     c                   IF        T97FUNZION = 'EXEC '
014900130429      **  se ha avuto dei problemi !!!!
015000130430     c                              and
015100130430     c                             T97ESITO <>'1'                                   x errori
015200130416      *
015300130415     c* Aggancia l'ORM per scrivere il messaggio
015400130415     c     kORM          chain     fnORM01l
015500130415     c*
015600130415     C                   if         %Found(FnORM01L)
015700130429     c                   exsr      write_Segm
015800130416     c                   else
015900130416      *    non ha trovato l'ORM
016000130429     c                   eval      T97ESITO = '1'                                  x errori
016100130416     c                   end
016200130416      *
016300130416     c                   end
016400130429      *   chiude in RT
016500130429     c                   seton                                        RT
016600130415      **?------------------------------------------------------------------ */
016700130429      **    IMPOSTA I CAMPI DELL'ORM NEL FORMATO GEL di NETEXPRESS EUROPE
016800130415      **?------------------------------------------------------------------ */
016900130429     C     write_Segm    Begsr
017000130429
017100130429      *   Toglie eventuali (;) dai campi onde evitare problemi
017200130429      *   nella scrittura del record.
017300130429     c     ';':' '       xlate     ORMrso        ORMrso
017400130429     c     ';':' '       xlate     ORMino        ORMino
017500130429     c     ';':' '       xlate     ORMcao        ORMcao
017600130429     c     ';':' '       xlate     ORMloo        ORMloo
017700130429     c     ';':' '       xlate     ORMpro        ORMpro
017800130429     c     ';':' '       xlate     ORMnao        ORMnao
017900130429      *
018000130429     c     ';':' '       xlate     ORMrsr        ORMrsr
018100130429     c     ';':' '       xlate     ORMinr        ORMinr
018200130429     c     ';':' '       xlate     ORMlor        ORMlor
018300130429     c     ';':' '       xlate     ORMcar        ORMcar
018400130429     c     ';':' '       xlate     ORMprr        ORMprr
018500130429     c     ';':' '       xlate     ORMnar        ORMnar
018600130429      *
018700130429     c     ';':' '       xlate     ORMrer        ORMrer
018800130429     c     ';':' '       xlate     ORMter        ORMter
018900130429      *
019000130429     c     ';':' '       xlate     ORMrsc        ORMrsc
019100130429     c     ';':' '       xlate     ORMinc        ORMinc
019200130429     c     ';':' '       xlate     ORMcac        ORMcac
019300130429     c     ';':' '       xlate     ORMloc        ORMloc
019400130429     c     ';':' '       xlate     ORMprc        ORMprc
019500130429     c     ';':' '       xlate     ORMnac        ORMnac
019600130429      *
019700130429     c     ';':' '       xlate     ORMnam        ORMnam
019800130429     c     ';':' '       xlate     ORMno1        ORMno1
019900130429     c     ';':' '       xlate     ORMno2        ORMno2
020000130429     c     ';':' '       xlate     ORMzor        ORMzor
020100130429     c     ';':' '       xlate     ORMrfa        ORMrfa
020200130429     c     ';':' '       xlate     ORMflo        ORMflo
020300130429      * _________________________
020400130429      *   pulisce la riga da riportare sul TIVGD
020500130429     c                   clear                   segmento
020600130429      * _________________________
020700130429      *  Inizio
020800130429     c                   eval      segmento  = 'P;DE1;;;'
020900130429      * _________________________
021000130430      *  rag.soc RITIRO
021100130429     c                   if        %len(%trim(ORMrsR)) <= 27
021200130429      *
021300130429     c                   eval      segmento  = %trim(segmento)
021400130429     c                              + %trim(ORMrsR)
021500130429     c                              + ';'
021600130429     c                   else
021700130429     c                   eval      segmento  = %trim(segmento)
021800130429     c                              + %subst(ORMrsR:1:27)
021900130429     c                              + ';'
022000130429     c                              + %Trim(%subst(ORMrsR:28:7))
022100130429     c                              + ';'
022200130429     c                   end
022300130429      * _________________________
022400130430      *  indiriz RITIRO
022500130429     c                   if        %len(%trim(ORMinR)) <= 27
022600130429     c                   eval      segmento  = %trim(segmento)
022700130429     c                              + %trim(ORMinR)
022800130429     c                              + ';'
022900130429     c                   else
023000130429     c                   eval      segmento  = %trim(segmento)
023100130429     c                              + %subst(ORMinR:1:27)
023200130429     c                              + ';'
023300130429     c                   end
023400130429      * _________________________
023500130430      *  Nazione RITIRO
023600130429     c                   eval      segmento  = %trim(segmento)
023700130429     c                              + %trim(ORMnaR)
023800130429     c                              + ';'
023900130429      * _________________________
024000130430      *  CAP     RITIRO
024100130429     c                   eval      segmento  = %trim(segmento)
024200130429     c                              + %trim(ORMcaR)
024300130429     c                              + ';'
024400130429      * _________________________
024500130430      *  Localit RITIRO
024600130429     c                   if        %len(%trim(ORMloR)) <= 27
024700130429     c                   eval      segmento  = %trim(segmento)
024800130429     c                              + %trim(ORMloR)
024900130429     c                              + ';'
025000130429     c                   else
025100130429     c                   eval      segmento  = %trim(segmento)
025200130429     c                              + %subst(ORMloR:1:27)
025300130429     c                              + ';'
025400130429     c                   end
025500130429      * _________________________
025600130430      *  Rif.    RITIRO
025700130429     c                   if        ormRFA =  *blank
025800130429     c                   eval      segmento  = %trim(segmento)
025900130429     c                              +     %editc(T97ORMPOE:'X')
026000130429     c                              +     %editc(T97ORMNSR:'X')
026100130429     c                              +     %editc(T97ORMNOR:'X')
026200130429     c                              +     %editc(T97ORMNRV:'X')
026300130429     c                              + ';'
026400130429     c                   else
026500130429     c                   eval      segmento  = %trim(segmento)
026600130429     c                              + %trim(ORMrfa)
026700130429     c                              + ';'
026800130429     c                   end
026900130429      * _________________________
027000130429      *  Data    PRONTA MERCE
027100130429     c                   eval      segmento  = %trim(segmento)
027200130523     c                              + %trim(%editc(ormDaR:'X'))
027300130429     c                              + ';'
027400130429      * _________________________
027500130429      *  rag.soc DESTINAT
027600130429     c                   if        %len(%trim(ORMrsC)) <= 27
027700130429      *
027800130429     c                   eval      segmento  = %trim(segmento)
027900130429     c                              + %trim(ORMrsC)
028000130429     c                              + ';'
028100130429     c                   else
028200130429     c                   eval      segmento  = %trim(segmento)
028300130429     c                              + %subst(ORMrsC:1:27)
028400130429     c                              + ';'
028500130429     c                              + %Trim(%subst(ORMrsC:28:7))
028600130429     c                              + ';'
028700130429     c                   end
028800130429      * _________________________
028900130429      *  indiriz DESTINAT
029000130429     c                   if        %len(%trim(ORMinC)) <= 27
029100130429     c                   eval      segmento  = %trim(segmento)
029200130429     c                              + %trim(ORMinC)
029300130429     c                              + ';'
029400130429     c                   else
029500130429     c                   eval      segmento  = %trim(segmento)
029600130429     c                              + %subst(ORMinC:1:27)
029700130429     c                              + ';'
029800130429     c                   end
029900130429      * _________________________
030000130429      *  Nazione DESTINAT
030100130429     c                   if        ormNAC <> *blank
030200130429     c                   eval      segmento  = %trim(segmento)
030300130429     c                              + %trim(ORMnaC)
030400130429     c                              + ';'
030500130429     c                   else
030600130429     c                   eval      segmento  = %trim(segmento)
030700130429     c                              + 'IT'
030800130429     c                              + ';'
030900130429     c                   end
031000130429      * _________________________
031100130429      *  CAP     DESTINAT
031200130429     c                   eval      segmento  = %trim(segmento)
031300130429     c                              + %trim(ORMcaC)
031400130429     c                              + ';'
031500130429      * _________________________
031600130429      *  Localit DESTINAT
031700130429     c                   if        %len(%trim(ORMloC)) <= 27
031800130429     c                   eval      segmento  = %trim(segmento)
031900130429     c                              + %trim(ORMloC)
032000130429     c                              + ';'
032100130429     c                   else
032200130429     c                   eval      segmento  = %trim(segmento)
032300130429     c                              + %subst(ORMloC:1:27)
032400130429     c                              + ';'
032500130429     c                   end
032600130429      * _________________________
032700130429      *  Nr      COLLI
032800130429     c                   eval      segmento  = %trim(segmento)
032900130523     c                              + %trim(%editc(ORMncl:'Z'))
033000130429     c                              + ';'
033100130429     c                              + ';'
033200130429     c                              + ';'
033300130429      * _________________________
033400130429      *  Kg      PESO x 10
033500130429     c                   eval      segmento  = %trim(segmento)
033600130523     c                              + %trim(%editc((ORMpkg*10):'Z'))
033700130429     c                              + ';'
033800130429      * _________________________
033900130429      *  Kg      VOLUME x 10
034000130429     c                   eval      segmento  = %trim(segmento)
034100130523     c                              + %trim(%editc(%int(ORMvlm*10):'Z'))
034200130429     c                              + ';O;;;;Y;;;;'
034300130429     c                              + ';'
034400130429      * _________________________
034500130429      *  (1)     NOTE
034600130429     c                   eval      segmento  = %trim(segmento)
034700130429     c                              + %trim(ORMno1)
034800130429     c                              + ';'
034900130429      * _________________________
035000130429      *  (2)     NOTE
035100130429     c                   eval      segmento  = %trim(segmento)
035200130429     c                              + %trim(ORMno2)
035300130429     c                              + ';'
035400130429      * _________________________
035500130429      *  DEPOT   DESTINAT
035600130429     c                   if        ormNAC <> *blank
035700130429     c                   eval      segmento  = %trim(segmento)
035800130429     c                              + 'DE1'
035900130429     c                              + ';'
036000130429     c                   else
036100130429     c                   eval      segmento  = %trim(segmento)
036200130429     c                              + 'IT1'
036300130429     c                              + ';'
036400130429     c                   end
036500130429      * _________________________
036600130429      *  DEPOT   ORDINANT
036700130429     c                   eval      segmento  = %trim(segmento)
036800130429     c                              + '380'
036900130429     c                              + ';'
037000130429      * _________________________
037100130429      *  REFERENCE
037200130429     c                   eval      segmento  = %trim(segmento)
037300130429     c                              +     %editc(T97ORMPOE:'X')
037400130429     c                              +     %editc(T97ORMNSR:'X')
037500130429     c                              +     %editc(T97ORMNOR:'X')
037600130429     c                              +     %editc(T97ORMNRV:'X')
037700130429     c                              + ';:00;:00;;;;'
037800130429      * _________________________
037900130429      *  REFERENTE
038000130429     c                   if        %len(%trim(ORMrer)) <= 20
038100130429     c                   eval      segmento  = %trim(segmento)
038200130429     c                              + %trim(ORMrer)
038300130429     c                              + ';'
038400130429     c                   else
038500130429     c                   eval      segmento  = %trim(segmento)
038600130429     c                              + %subst(ORMrer:1:20)
038700130429     c                              + ';'
038800130429     c                   end
038900130429      * _________________________
039000130429      *  TEL.REFERENTE
039100130429     c                   eval      segmento  = %trim(segmento)
039200130429     c                              + %trim(ORMter)
039300130429     c                              + ';'
039400130429      * _________________________
039500130418      **  [scrive TIVGD]
039600130429     c                   exsr      VGD_WRITE
039700130415
039800130415     C                   Endsr
039900110405      **?__________________________________________________________________ */
040000110405      *    Apertura del TIVGD
040100110316      **?__________________________________________________________________ */
040200110405     C     VGD_OPEN      Begsr
040300110405      *
040400110617      *  recupera il progressivo
040500110617     c                   exsr      Piglia_progr
040600110617      *
040700110405      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
040800110405     C                   clear                   trul47ds
040900110405     C                   eval      d47opz  = 'I'
041000110617     C                   eval      d47tip  = TipoF
041100110405     C                   eval      d47lck  = 'N'
041200110405     C                   eval      d47chkj = 'N'
041300130429     C                   eval      d47pgm  = 'TRTCT97R'
041400110405     C                   call      'TRUL47R'
041500110405     C                   parm                    trul47ds
041600110405      *
041700110405     c                   endsr
041800130429      **?__________________________________________________________________ */
041900130429      *   Prende il Progressivo
042000130429      **?__________________________________________________________________ */
042100130429     C     Piglia_progr  Begsr
042200130429      *
042300130521     C                   MOVE      CLIENTE_trs   virKSC
042400130429     C                   MOVEL     TipoF         virTIP
042500130429     c     kvir02        chain     tivir02l
042600130429     c                   move      'OF'          Var_ISV
042700130429     c                   if        %Found(tivir02l)
042800130429     c                   move      VirFI1        Var_ISV
042900130429     C                   End
043000130429
043100130429      *       prende numeratore strategi
043200130429     C                   exsr      calprog
043300130429      *
043400130429      *  Se non ha reperito correttamente il prograssivo
043500130429      *  per la scrittura del TIVGD, potrebbe essere stato richiamato male
043600130429     C                   if        sav_PROGR   = *blank
043700130429???? C                   EVAL      Indirizzi = CED_Bart
043800130429???? C                   EVAL      Oggetto ='Problemi ORM DOWNLOAD x GEL'
043900130429     C                   EVAL      Messaggio = 'ATTENZIONE : -
044000130429     C                             NON agganciato PROGRESSIVO x il TIPO In-
044100130429     C                             vio su DOWNLOAD.'
044200130429     c                   exsr      Invio_Mail
044300130429     c                   eval      T97ESITO = '1'
044400130429     c                   end
044500130429      *
044600130429     c                   endsr
044700130429     C*------------------------------------------------------*
044800130429     C*  prepara il file
044900130429     C*------------------------------------------------------*
045000130429     C     calprog       begsr
045100130429     C*
045200130429     C                   open      tis7prgf
045300130429     C                   read(e)   tis7prgf
045400130429     C*
045500130429     C                   if        not %error
045600130429     C                   eval      dwlprg = f_tis7prgf
045700130429     C*
045800130429     C                   move(p)   dwlprg        wrkprg
045900130429     C                   add       1             wrkprg
046000130429     C                   move(p)   wrkprg        dwlprg
046100130429     C                   movel     Var_ISV       dwlprg
046200130429     C*
046300130429     C                   eval       f_tis7prgf = dwlprg
046400130429     c                   eval      SAV_Progr   = dwlprg
046500130429     C*
046600130429     C                   update    tis7prg0
046700130429     C                   endif
046800130429     C*
046900130429     C                   close     tis7prgf
047000130429     C*
047100130429     C                   endsr
047200110617      **?__________________________________________________________________ */
047300110617      *   Scrittura del TIVGD
047400110617      **?__________________________________________________________________ */
047500110617     C     VGD_WRITE     Begsr
047600110617      *
047700110405     c                   clear                   tivgd000
047800110405     c                   eval      vgddta = %TrimR(Segmento)
047900110617     c                   eval      vgdtip = TipoF
048000130521     c                   eval      vgdksu = CLIENTE_trs
048100130430     C                   eval      vgdprg = SAV_Progr
048200110405     c                   eval      vgdtsc = 'WW'
048300130429     c                   eval      vgdpgm = 'TRTCT97R'
048400110405     c                   eval      vgddat = udtymd
048500110405      *
048600110405     C                   WRITE     tivgd000
048700110405      *
048800110405     c                   endsr
048900110405      **?__________________________________________________________________ */
049000110405      *    Chiusura del TIVGD
049100110405      **?__________________________________________________________________ */
049200110405     C     VGD_CLOSE     Begsr
049300110405      *
049400110405     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
049500110405     C                   clear                   trul47ds
049600110405     C                   eval      d47opz  = 'F'
049700110617     C                   eval      d47tip  = TipoF
049800110405     C                   call      'TRUL47R'
049900110405     C                   parm                    trul47ds
050000110405      *
050100110405     c                   endsr
050200110617      **?__________________________________________________________________ */
050300110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
050400130415      **?__________________________________________________________________ */
050500110621     C     *pssr         BEGSR
050600110621     C
050700130429     C                   eval      T97ESITO = '1'
050800110621     C                   ENDSR     '*CANCL'
050900110621     C
051000130415      * ?------------------------------------------------------------------ */
051100130415      * CTRL caricamento schiere    PT                               -*
051200130415      * ?------------------------------------------------------------------ */
051300130415     C     Check_PT      begsr
051400130415     C*
051500130415     c* Controllo riempimento schiera
051600130415     c                   clear                   trul0sds
051700130415     c                   eval      i0sski='PT_key'
051800130415     c                   eval      i0sele=%elem(PT_key)
051900130415     c                   eval      i0spie=x
052000130415     c                   eval      i0sfile='EDTAB00F'
052100130415     c                   eval      i0ssif=knsif
052200130429     c                   eval      i0spgm='TRTCT97R'
052300130415     c                   move      kpjbu         SvKpjbu
052400130415     c                   clear                   kpjbu
052500130415     c                   movel     trul0sds      kpjbu
052600130415     c                   call      'TRUL0SR'
052700130415     c                   parm                    kpjba
052800130415     c                   eval      kpjbu = SvKpjbu
052900130415     C*
053000130415     C                   ENDSR
053100130415      * ?------------------------------------------------------------------ */
053200130429      *?    CARICA TABELLE SU SCHIERE con il SOLO record INTERESSATO
053300130429      *  anche se probabilmente non vengono utilizzate le informazioni delle
053400130429      *  tabelle, sono pronte per essere utilizzate su eventuali modifiche future
053500130415      * ?------------------------------------------------------------------ */
053600130429     C     CARICA_EDTAB  BEGSR
053700130415      *
053800130415      * Caricamento Tabella Partner esteri
053900130429     C                   Z-ADD     0             X                 5 0
054000130415     C                   MOVEL     'PT'          TABCOD
054100130429     C     TABCOD        setll     EDTAB01L
054200130429     C     TABCOD        READE     EDTAB01L                               30
054300130429     C                   DOW       not %EoF(EDTAB01L)
054400130429     C                   MOVEL     TABUNI        edidsPT
054500130416      *
054600130415      * carica solo quello che interessa uno .... due ....tre record al max.
054700130429     C                   IF        TABFLG = *BLANKS  and
054800130429     C                               cliente7n = §PTKSC
054900130429      *   Carica solo il PRIMO TROVATO
055000130415     C                   ADD       1             X
055100130415     C                   MOVEL     TABKEY        PT_key(X)
055200130415     C                   MOVEL     TABUNI        PT_uni(X)
055300130416     c                   LEAVE
055400130415     C                   END
055500130429      *
055600130415     C     TABCOD        READE     EDTAB01L                               30
055700130415     C                   END
055800130415      *
055900130416      *  se non ha trovato nemmeno un record sulla PT lo segnala
056000130416     C                   IF            X = 0
056100130416???? C                   EVAL      Indirizzi = CED_Bart
056200130429???? C                   EVAL      Oggetto ='Problemi ORM DOWNLOAD x GEL'
056300130429     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione GEL -
056400130416     C                             ORM EXPORT - messaggio -
056500130429     C                             NON TROVATA tabella PT controllare EDI -
056600130429     C                             su DOWNLOAD.'
056700130416     c                   exsr      Invio_Mail
056800130429     c                   eval      T97ESITO = '1'
056900130416     C                   END
057000130416      *
057100130415      *  se deve inviare la mail di alert x limite quasi raggiunto.
057200130415     c                   exsr      ChecK_PT
057300130415      *
057400130415      * salva quanti Codici UNB ha caricato
057500130415     C                   MOVEL     'PU'          TABCOD
057600130416     C     KediTAB       CHAIN     EDTAB01L
057700130416     C                   if        %Found(EDTAB01L) and
057800130416     C                             TABFLG = *BLANKS
057900130416     C                   MOVEL     TABUNI        edidsPU
058000130418     c                   movel     §pucompat     Compatta_80       1
058100130415     C                   MOVEL     TABKEY        PU_key
058200130415     C                   Z-ADD     1             X
058300130415     C     PU_key        LOOKUP    PT_key(X)                              32
058400130415     C   32              MOVEL     TABUNI        PU_uni(X)
058500130415     C                   END
058600130415      *
058700130415      * Prosegue tab.PT e PU
058800130415     C                   MOVEL     'PV'          TABCOD
058900130416     C     KediTAB       CHAIN     EDTAB01L
059000130416     C                   if        %Found(EDTAB01L) and
059100130416     C                             TABFLG = *BLANKS
059200130416     C                   MOVEL     TABUNI        edidsPV
059300130415     C                   MOVEL     TABKEY        PV_key
059400130415     C                   Z-ADD     1             X
059500130415     C     PV_key        LOOKUP    PT_key(X)                              32
059600130415     C   32              MOVEL     TABUNI        PV_uni(X)
059700130416      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
059800130416      *  ricevuti:
059900130416     c                   movel     §PVema        mail_ind         44
060000130416      * ?imposta gli indirizzi mail se interni a BRT o esterni
060100130416     c                   if        mail_ind <> *blank
060200130416     c                   exsr      imp_ADDR_mail
060300130416     c                   end
060400130416      *
060500130416     C                   END
060600130415      *
060700130415      * Prosegue tab.PT e PU e PV
060800130415     C                   MOVEL     'PZ'          TABCOD
060900130416     C     KediTAB       CHAIN     EDTAB01L
061000130416     C                   if        %Found(EDTAB01L) and
061100130416     C                             TABFLG = *BLANKS
061200130416     C                   MOVEL     TABUNI        edidsPZ
061300130415     C                   MOVEL     TABKEY        PZ_key
061400130415     C                   Z-ADD     1             X
061500130415     C     PZ_key        LOOKUP    PT_key(X)                              32
061600130415     C   32              MOVEL     TABUNI        PZ_uni(X)
061700130521      **
061800130521      * se richiesta la trasmissione ad un codice UNIFICANTE PARTICOLARE
061900130521      **   deve sovrapporsi al codice cliente Generico legato all'UNB
062000130521     c                   if                  §puXXX = 'S' and
062100130521     c                                §PZFTPCOD <> *zeros and
062200130521     c                                §PZFTPCOD <> *blank
062300130521     c                   move      *all'0'       CLIENTE_trs
062400130521     c                   move      §PZFTPCOD     CLIENTE_trs
062500130521     c                   end
062600130521      *
062700130415     C                   END
062800130523      *
062900130523      * Se invece NON aveva questo tipo di invio con ALTRO CODICE imposta
063000130523      *  il codice cliente della TABELLA PT generale come codice trasmissione
063100130523     c                   if        CLIENTE_trs = *blank
063200130523     C                   MOVEL     CLIENTE8a     CLIENTE_trs
063300130523     c                   end
063400130415      *
063500130419      * Prosegue tab.PT e PU e PV e PO
063600130419     C                   MOVEL     'PO'          TABCOD
063700130419     C     KediTAB       CHAIN     EDTAB01L
063800130419     C                   if        %Found(EDTAB01L) and
063900130419     C                             TABFLG = *BLANKS
064000130419     C                   MOVEL     TABUNI        edidsPO
064100130419     C                   MOVEL     TABKEY        PO_key
064200130419     C                   Z-ADD     1             X
064300130419     C     PO_key        LOOKUP    PT_key(X)                              32
064400130419     C   32              MOVEL     TABUNI        PO_uni(X)
064500130419     C                   END
064600130415      *
064700130415     C                   ENDSR
064800130416     c*==================================================================*
064900130416      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
065000130416     c*==================================================================*
065100130416     c     Imp_ADDR_mail begsr
065200130416      *
065300130416     c                   clear                   Indirizzi
065400130416      *
065500130416      *  Lunghezza indirizzi presenti
065600130416     c                   eval      Lunghezza_Indirizzo =
065700130416     c                                       %Len(%TrimR(Mail_Ind))
065800130416     c                   z-add     1             al_char           3 0
065900130416     c                   z-add     1             dal_char          3 0
066000130416     c                   z-add     1             tot_char          3 0
066100130416      *
066200130416     c     successivo    tag
066300130416      *
066400130416      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
066500130416      *   e aggiunge il dominio Brt + il (;) separatore
066600130416     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
066700130416     c                   eval      tot_char = al_char - dal_char
066800130416     c                   z-add     dal_char      dalCarattere
066900130416     c                   z-add     tot_char      xTOTcaratteri
067000130416      *
067100130416     c                   clear                   Indir_BRT        40
067200130416     c                   clear                   Indir_Parz       20
067300130416     c                   eval      Indir_Parz =
067400130416     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
067500130416      *
067600130416     c                   eval      Indir_BRT    = %Trim(Indir_Parz) +
067700130416     c                             %Trim(bart_it)
067800130416     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_BRT
067900130416      *
068000130416     c                   if        al_char = Lunghezza_Indirizzo
068100130416     c                   goto      fine_indmail
068200130416     c                   else
068300130416     c                   eval      dal_char = ( al_char + 1 )
068400130416     c                   goto      successivo
068500130416     c                   end
068600130416      *
068700130416     C     fine_indmail  TAG
068800130416      *
068900130416     C                   ENDSR
069000130416     c*==================================================================*
069100130416      * Manda un Msg x E-mail
069200130416     c*==================================================================*
069300130416     c     Invio_Mail    begsr
069400130416      *
069500130416     C*   Alert_mail : invio Mail a CED@Brt.it                  *
069600130416     C* Inizializzo variabili
069700130416     C                   movel     *blanks       wrkEml          253
069800130416     C                   movel     *blanks       wrkMsg         5000
069900130416     C                   movel     *blanks       wrkOgg           44
070000130416      *
070100130416     C* Valorizzo i campi della e-m@ail - indirizzo
070200130416     C*******            eval      wrkEml = 'Andrea.Bertocchi@Brt.it'
070300130416     C                   eval      wrkEml = Indirizzi
070400130416     C                   eval      wrkOgg = Oggetto
070500130416     C                   eval      wrkMsg = Messaggio
070600130416      ** *****
070700130416     C                   call(e)   'TRTCT00R2'
070800130416     C                   parm                    wrkEml
070900130416     C                   parm                    wrkOgg
071000130416     C                   parm                    wrkMsg
071100130416     C*
071200130416     C                   ENDSR
071300130429      * ?------------------------------------------------------------------ */
071400130429     C* Solo la prima volta  aprendo in RT
071500130415      * ?------------------------------------------------------------------ */
071600130429     C     *inzsr        begSR
071700130429     C*
071800130429     C                   ENDSR
071900130429      * ?------------------------------------------------------------------ */
