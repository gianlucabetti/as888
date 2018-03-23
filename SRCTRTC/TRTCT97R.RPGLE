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
003100131030      * Numeratore ORM (311)
003200131030     D trul33ds      E DS
003300130415      *
003400130415     D TRUL0SDS      E DS
003500130415      *
003600130415      * Tabella partner
003700130415     D PT_key          S             35    DIM(200)
003800130415     D PT_uni          S             90    DIM(%elem(PT_key))
003900130415     D PU_uni          S             90    DIM(%elem(PT_key))
004000130415     D PV_uni          S             90    DIM(%elem(PT_key))
004100130415     D PZ_uni          S             90    DIM(%elem(PT_key))
004200130419     D PO_uni          S             90    DIM(%elem(PT_key))
004300130415      *
004400130415     D PU_key          S             35
004500130415     D PV_key          S             35
004600130415     D PZ_key          S             35
004700130419     D PO_key          S             35
004800130429      *
004900130430     D SAV_Progr       S                   like(vgdPRG)
005000130415      * ?================================================================== */
005100130416      *  Invio E-mail
005200130416     D Indirizzi       s            253
005300130416     D Oggetto         s             44
005400130416     D Messaggio       s           5000
005500130416     d   DSmail        ds
005600130416     D Mail_Ind                      44
005700130416     d   IND_char                     1    dim(44)
005800130416      *
005900130416     D Lunghezza_Indirizzo...
006000130416     D                 s              5u 0
006100130416     D dalCarattere    s              5u 0
006200130416     D xTOTcaratteri   s              5u 0
006300130416      *
006400130416     d bart_it         C                   CONST('@Brt.it;')
006500130416     d CED_Bart        C                   CONST('CED@Brt.it;')
006600130415      *---------------------------------------------------------------------*
006700130429     D TipoF           C                   CONST('EM')
006800110617     D dwlprg          s             10    INZ(*all'0')
006900110617     D wrkprg          s              8  0 INZ(*zeros)
007000060814     d trul47ds      e ds
007100060814     D W0140           S             14  0 inz
007200110316     D Segmento        s           2048
007300110316     D Conta_righe     s              5i 0
007400131030     D NumProgr        s              8s 0 INZ(*zeros)
007500110617      *---------------------------------------------------------------------*
007600110617     D Var_ISV         s                   like(Virfi1)
007700050927      **?__________________________________________________________________ */
007800050704      *    main
007900110316      **?__________________________________________________________________ */
008000130415     c     *entry        plist
008100130415     C                   PARM                    kpjba
008200130520     C                   movel     kpjbu         trtctORMds
008300130416     c*
008400130430     c*  DEVE essere richiamato correttamente con la FUNZIONE apposita
008500130429     c                   clear                   T97ESITO          1
008600130430     c                   IF        T97FUNZION <>'OPEN ' and
008700130430     c                             T97FUNZION <>'EXEC ' and
008800130430     c                             T97FUNZION <>'CLOSE'
008900130430     c                   move      '1'           T97ESITO
009000130430???? C                   EVAL      Indirizzi = CED_Bart
009100130430???? C                   EVAL      Oggetto ='Problemi ORM DOWNLOAD x GEL'
009200130430     C                   EVAL      Messaggio = 'ATTENZIONE : -
009300130430     C                             il TRTCT97R è stato richiamato in MODO -
009400130430     C                             anomalo. NON è stata passata la corrett-
009500130430     C                             a modalità: OPEN/EXEC/CLOSE. Controllar-
009600130430     c                             e il programma Chiamante e corregerlo!'
009700130430     c                   exsr      Invio_Mail
009800130430     c                   SETON                                        LR
009900130430     c                   RETURN
010000130430     c                   end
010100130429      *
010200130416     c     kediTAB       klist
010300130416     C                   kfld                    tabCOD
010400130416     C                   kfld                    tabKEY
010500130415     c*
010600130415     c     kORM          klist
010700130429     C                   kfld                    T97ORMPOE
010800130429     C                   kfld                    T97ORMNSR
010900130429     C                   kfld                    T97ORMNOR
011000130429     C                   kfld                    T97ORMNRV
011100130415     c*
011200110617     c     kvir02        klist
011300110617     C                   kfld                    virKSC
011400110617     C                   kfld                    virTIP
011500130415      *
011600130429      *  Se deve INIZIARE passa l' OPEN
011700130429      * ?              /*--------------------------- */
011800130429     c                   IF        T97FUNZION = 'OPEN '
011900130429     c                               or
012000130429     C                             SAV_Progr   = *blank
012100130429      * ?              /*--------------------------- */
012200130429      *
012300130429     C                   TIME                    ORADAT           14 0
012400130429     C                   MOVEL     ORADAT        ORATR             6 0
012500130429     c                   move      *date         udtymd            8 0
012600130429     C                   TIME                    W0140
012700130429     C                   MOVE      W0140         W0080             8 0
012800130429     C                   MOVE      T97CLIENTE    Cliente7n         7 0
012900130430     C                   MOVE      *zeros        CLIENTE8A         8
013000130430     C                   MOVE      T97CLIENTE    CLIENTE8A
013100130521     c                   move      *blanks       CLIENTE_trs       8
013200130521      *
013300130521     c                   EXSR      vgd_OPEN
013400130521      * ?              /*--------------------------- */
013500130429     c                   exsr      CARICA_EDTAB
013600130521      * ?              /*--------------------------- */
013700130429      **
013800130430      * quindi chiude e conferma la scrittura del TIVGD
013900130429      * ?              /*--------------------------- */
014000130429     c                   elseIF    T97FUNZION = 'CLOSE'
014100130429      * ?              /*--------------------------- */
014200130429     c                   commit
014300130429     C                   EXSR      vgd_CLOSE
014400130429     c                   seton                                        LR
014500130429     c                   RETURN
014600130429      **
014700130429      * ?              /*--------------------------- */
014800130429     c                   end
014900130429      * ?              /*--------------------------- */
015000130429      **
015100130430     c                   IF        T97FUNZION = 'EXEC '
015200130429      **  se ha avuto dei problemi !!!!
015300130430     c                              and
015400130430     c                             T97ESITO <>'1'                                   x errori
015500130416      *
015600130415     c* Aggancia l'ORM per scrivere il messaggio
015700130415     c     kORM          chain     fnORM01l
015800130415     c*
015900130415     C                   if         %Found(FnORM01L)
016000130429     c                   exsr      write_Segm
016100130416     c                   else
016200130416      *    non ha trovato l'ORM
016300130429     c                   eval      T97ESITO = '1'                                  x errori
016400130416     c                   end
016500130416      *
016600130416     c                   end
016700130429      *   chiude in RT
016800130429     c                   seton                                        RT
016900130415      **?------------------------------------------------------------------ */
017000130429      **    IMPOSTA I CAMPI DELL'ORM NEL FORMATO GEL di NETEXPRESS EUROPE
017100130415      **?------------------------------------------------------------------ */
017200130429     C     write_Segm    Begsr
017300131030      *
017400131030     C*     => Stacco un numeratore da AZNUM
017500131030     c                   z-add     1600001       NUMprogr
017600131030     c                   movel     kpjbu         svkpjbu
017700131030     c                   clear                   kpjbu
017800131030     C                   clear                   TRUL33DS
017900131030     C                   eval      I33OPE = *zeros
018000131030     C                   eval      I33CNU = 311
018100131030     C                   eval      I33NUM = 1
018200131030     C                   movel     TRUL33DS      KPJBU
018300131030     C                   call      'TRUL33R'
018400131030     C                   parm                    KPJBA
018500131030     C                   movel     KPJBU         TRUL33DS
018600131030     c                   movel     svkpjbu       kpjbu
018700131030      ****
018800131030     C                   if        O33ERR = *zeros
018900131030     c                   z-add     o33nrf        NUMprogr
019000131030     C                   else
019100131030     C                   endif
019200131030      ****
019300130429
019400130429      *   Toglie eventuali (;) dai campi onde evitare problemi
019500130429      *   nella scrittura del record.
019600130429     c     ';':' '       xlate     ORMrso        ORMrso
019700130429     c     ';':' '       xlate     ORMino        ORMino
019800130429     c     ';':' '       xlate     ORMcao        ORMcao
019900130429     c     ';':' '       xlate     ORMloo        ORMloo
020000130429     c     ';':' '       xlate     ORMpro        ORMpro
020100130429     c     ';':' '       xlate     ORMnao        ORMnao
020200130429      *
020300130429     c     ';':' '       xlate     ORMrsr        ORMrsr
020400130429     c     ';':' '       xlate     ORMinr        ORMinr
020500130429     c     ';':' '       xlate     ORMlor        ORMlor
020600130429     c     ';':' '       xlate     ORMcar        ORMcar
020700130429     c     ';':' '       xlate     ORMprr        ORMprr
020800130429     c     ';':' '       xlate     ORMnar        ORMnar
020900130429      *
021000130429     c     ';':' '       xlate     ORMrer        ORMrer
021100130429     c     ';':' '       xlate     ORMter        ORMter
021200130429      *
021300130429     c     ';':' '       xlate     ORMrsc        ORMrsc
021400130429     c     ';':' '       xlate     ORMinc        ORMinc
021500130429     c     ';':' '       xlate     ORMcac        ORMcac
021600130429     c     ';':' '       xlate     ORMloc        ORMloc
021700130429     c     ';':' '       xlate     ORMprc        ORMprc
021800130429     c     ';':' '       xlate     ORMnac        ORMnac
021900130429      *
022000130429     c     ';':' '       xlate     ORMnam        ORMnam
022100130429     c     ';':' '       xlate     ORMno1        ORMno1
022200130429     c     ';':' '       xlate     ORMno2        ORMno2
022300130429     c     ';':' '       xlate     ORMzor        ORMzor
022400130429     c     ';':' '       xlate     ORMrfa        ORMrfa
022500130429     c     ';':' '       xlate     ORMflo        ORMflo
022600130429      * _________________________
022700130429      *   pulisce la riga da riportare sul TIVGD
022800130429     c                   clear                   segmento
022900130429      * _________________________
023000130429      *  Inizio
023100130429     c                   eval      segmento  = 'P;DE1;;;'
023200130429      * _________________________
023300130430      *  rag.soc RITIRO
023400130429     c                   if        %len(%trim(ORMrsR)) <= 27
023500130429      *
023600130429     c                   eval      segmento  = %trim(segmento)
023700130429     c                              + %trim(ORMrsR)
023800130913     c                              + ';;'
023900130429     c                   else
024000130429     c                   eval      segmento  = %trim(segmento)
024100130429     c                              + %subst(ORMrsR:1:27)
024200130429     c                              + ';'
024300130429     c                              + %Trim(%subst(ORMrsR:28:7))
024400130429     c                              + ';'
024500130429     c                   end
024600130429      * _________________________
024700130430      *  indiriz RITIRO
024800130429     c                   if        %len(%trim(ORMinR)) <= 27
024900130429     c                   eval      segmento  = %trim(segmento)
025000130429     c                              + %trim(ORMinR)
025100130913     c                              + ';'
025200130429     c                   else
025300130429     c                   eval      segmento  = %trim(segmento)
025400130429     c                              + %subst(ORMinR:1:27)
025500130429     c                              + ';'
025600130429     c                   end
025700130429      * _________________________
025800130430      *  Nazione RITIRO
025900130429     c                   eval      segmento  = %trim(segmento)
026000130429     c                              + %trim(ORMnaR)
026100130429     c                              + ';'
026200130429      * _________________________
026300130430      *  CAP     RITIRO
026400130429     c                   eval      segmento  = %trim(segmento)
026500130429     c                              + %trim(ORMcaR)
026600130429     c                              + ';'
026700130429      * _________________________
026800130430      *  Localit RITIRO
026900130429     c                   if        %len(%trim(ORMloR)) <= 27
027000130429     c                   eval      segmento  = %trim(segmento)
027100130429     c                              + %trim(ORMloR)
027200130429     c                              + ';'
027300130429     c                   else
027400130429     c                   eval      segmento  = %trim(segmento)
027500130429     c                              + %subst(ORMloR:1:27)
027600130429     c                              + ';'
027700130429     c                   end
027800130429      * _________________________
027900130430      *  Rif.    RITIRO
028000131009     c**********         if        ormRFA =  *blank
028100130429     c                   eval      segmento  = %trim(segmento)
028200130429     c                              +     %editc(T97ORMPOE:'X')
028300130429     c                              +     %editc(T97ORMNSR:'X')
028400130429     c                              +     %editc(T97ORMNOR:'X')
028500130429     c                              +     %editc(T97ORMNRV:'X')
028600130429     c                              + ';'
028700131009     c**********         else
028800131009     c**********         eval      segmento  = %trim(segmento)
028900131009     c**********                    + %trim(ORMrfa)
029000131009     c**********                    + ';'
029100131009     c**********         end
029200130429      * _________________________
029300130429      *  Data    PRONTA MERCE
029400130429     c                   eval      segmento  = %trim(segmento)
029500130523     c                              + %trim(%editc(ormDaR:'X'))
029600130429     c                              + ';'
029700130429      * _________________________
029800130429      *  rag.soc DESTINAT
029900130429     c                   if        %len(%trim(ORMrsC)) <= 27
030000130429      *
030100130429     c                   eval      segmento  = %trim(segmento)
030200130429     c                              + %trim(ORMrsC)
030300130913     c                              + ';;'
030400130429     c                   else
030500130429     c                   eval      segmento  = %trim(segmento)
030600130429     c                              + %subst(ORMrsC:1:27)
030700130429     c                              + ';'
030800130429     c                              + %Trim(%subst(ORMrsC:28:7))
030900130429     c                              + ';'
031000130429     c                   end
031100130429      * _________________________
031200130429      *  indiriz DESTINAT
031300130429     c                   if        %len(%trim(ORMinC)) <= 27
031400130429     c                   eval      segmento  = %trim(segmento)
031500130429     c                              + %trim(ORMinC)
031600130429     c                              + ';'
031700130429     c                   else
031800130429     c                   eval      segmento  = %trim(segmento)
031900130429     c                              + %subst(ORMinC:1:27)
032000130429     c                              + ';'
032100130429     c                   end
032200130429      * _________________________
032300130429      *  Nazione DESTINAT
032400130429     c                   if        ormNAC <> *blank
032500130429     c                   eval      segmento  = %trim(segmento)
032600130429     c                              + %trim(ORMnaC)
032700130429     c                              + ';'
032800130429     c                   else
032900130429     c                   eval      segmento  = %trim(segmento)
033000130429     c                              + 'IT'
033100130429     c                              + ';'
033200130429     c                   end
033300130429      * _________________________
033400130429      *  CAP     DESTINAT
033500130429     c                   eval      segmento  = %trim(segmento)
033600130429     c                              + %trim(ORMcaC)
033700130429     c                              + ';'
033800130429      * _________________________
033900130429      *  Localit DESTINAT
034000130429     c                   if        %len(%trim(ORMloC)) <= 27
034100130429     c                   eval      segmento  = %trim(segmento)
034200130429     c                              + %trim(ORMloC)
034300130429     c                              + ';'
034400130429     c                   else
034500130429     c                   eval      segmento  = %trim(segmento)
034600130429     c                              + %subst(ORMloC:1:27)
034700130429     c                              + ';'
034800130429     c                   end
034900130429      * _________________________
035000130429      *  Nr      COLLI
035100130429     c                   eval      segmento  = %trim(segmento)
035200130523     c                              + %trim(%editc(ORMncl:'Z'))
035300130429     c                              + ';'
035400130429     c                              + ';'
035500130429     c                              + ';'
035600130429      * _________________________
035700130429      *  Kg      PESO x 10
035800130429     c                   eval      segmento  = %trim(segmento)
035900131218     c                              + %trim(%editc((ORMpkg):'Z'))
036000130429     c                              + ';'
036100130429      * _________________________
036200130429      *  Kg      VOLUME x 10
036300130429     c                   eval      segmento  = %trim(segmento)
036400131218     c                              + %trim(%editc(%int(ORMvlm):'Z'))
036500130429     c                              + ';O;;;;Y;;;;'
036600130429     c                              + ';'
036700130429      * _________________________
036800130429      *  (1)     NOTE
036900130429     c                   eval      segmento  = %trim(segmento)
037000130429     c                              + %trim(ORMno1)
037100130429     c                              + ';'
037200130429      * _________________________
037300130429      *  (2)     NOTE
037400130429     c                   eval      segmento  = %trim(segmento)
037500130429     c                              + %trim(ORMno2)
037600130429     c                              + ';'
037700130429      * _________________________
037800130429      *  DEPOT   DESTINAT
037900130429     c                   if        ormNAC <> *blank
038000130429     c                   eval      segmento  = %trim(segmento)
038100130429     c                              + 'DE1'
038200130429     c                              + ';'
038300130429     c                   else
038400130429     c                   eval      segmento  = %trim(segmento)
038500130429     c                              + 'IT1'
038600130429     c                              + ';'
038700130429     c                   end
038800130429      * _________________________
038900130429      *  DEPOT   ORDINANT
039000130429     c                   eval      segmento  = %trim(segmento)
039100130913     c                              + 'IT1'
039200130429     c                              + ';'
039300130429      * _________________________
039400131030      *  REFERENCE   (progressivo composto da 0160+ numeratore di 4 cifre da 0001 a 9999)
039500130429     c                   eval      segmento  = %trim(segmento)
039600131030     c                              +     %editc(numProgr:'X')
039700130919     c                              + ';;;;;;'
039800130429      * _________________________
039900130429      *  REFERENTE
040000130429     c                   if        %len(%trim(ORMrer)) <= 20
040100130429     c                   eval      segmento  = %trim(segmento)
040200130429     c                              + %trim(ORMrer)
040300130429     c                              + ';'
040400130429     c                   else
040500130429     c                   eval      segmento  = %trim(segmento)
040600130429     c                              + %subst(ORMrer:1:20)
040700130429     c                              + ';'
040800130429     c                   end
040900130429      * _________________________
041000130429      *  TEL.REFERENTE
041100130429     c                   eval      segmento  = %trim(segmento)
041200130429     c                              + %trim(ORMter)
041300130429     c                              + ';'
041400130429      * _________________________
041500130418      **  [scrive TIVGD]
041600130429     c                   exsr      VGD_WRITE
041700130415
041800130415     C                   Endsr
041900110405      **?__________________________________________________________________ */
042000110405      *    Apertura del TIVGD
042100110316      **?__________________________________________________________________ */
042200110405     C     VGD_OPEN      Begsr
042300110405      *
042400110617      *  recupera il progressivo
042500110617     c                   exsr      Piglia_progr
042600110617      *
042700110405      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
042800110405     C                   clear                   trul47ds
042900110405     C                   eval      d47opz  = 'I'
043000110617     C                   eval      d47tip  = TipoF
043100110405     C                   eval      d47lck  = 'N'
043200110405     C                   eval      d47chkj = 'N'
043300130429     C                   eval      d47pgm  = 'TRTCT97R'
043400110405     C                   call      'TRUL47R'
043500110405     C                   parm                    trul47ds
043600110405      *
043700110405     c                   endsr
043800130429      **?__________________________________________________________________ */
043900130429      *   Prende il Progressivo
044000130429      **?__________________________________________________________________ */
044100130429     C     Piglia_progr  Begsr
044200130429      *
044300130521     C                   MOVE      CLIENTE_trs   virKSC
044400130429     C                   MOVEL     TipoF         virTIP
044500130429     c     kvir02        chain     tivir02l
044600130429     c                   move      'OF'          Var_ISV
044700130429     c                   if        %Found(tivir02l)
044800130429     c                   move      VirFI1        Var_ISV
044900130429     C                   End
045000130429
045100130429      *       prende numeratore strategi
045200130429     C                   exsr      calprog
045300130429      *
045400130429      *  Se non ha reperito correttamente il prograssivo
045500130429      *  per la scrittura del TIVGD, potrebbe essere stato richiamato male
045600130429     C                   if        sav_PROGR   = *blank
045700130429???? C                   EVAL      Indirizzi = CED_Bart
045800130429???? C                   EVAL      Oggetto ='Problemi ORM DOWNLOAD x GEL'
045900130429     C                   EVAL      Messaggio = 'ATTENZIONE : -
046000130429     C                             NON agganciato PROGRESSIVO x il TIPO In-
046100130429     C                             vio su DOWNLOAD.'
046200130429     c                   exsr      Invio_Mail
046300130429     c                   eval      T97ESITO = '1'
046400130429     c                   end
046500130429      *
046600130429     c                   endsr
046700130429     C*------------------------------------------------------*
046800130429     C*  prepara il file
046900130429     C*------------------------------------------------------*
047000130429     C     calprog       begsr
047100130429     C*
047200130429     C                   open      tis7prgf
047300130429     C                   read(e)   tis7prgf
047400130429     C*
047500130429     C                   if        not %error
047600130429     C                   eval      dwlprg = f_tis7prgf
047700130429     C*
047800130429     C                   move(p)   dwlprg        wrkprg
047900130429     C                   add       1             wrkprg
048000130429     C                   move(p)   wrkprg        dwlprg
048100130429     C                   movel     Var_ISV       dwlprg
048200130429     C*
048300130429     C                   eval       f_tis7prgf = dwlprg
048400130429     c                   eval      SAV_Progr   = dwlprg
048500130429     C*
048600130429     C                   update    tis7prg0
048700130429     C                   endif
048800130429     C*
048900130429     C                   close     tis7prgf
049000130429     C*
049100130429     C                   endsr
049200110617      **?__________________________________________________________________ */
049300110617      *   Scrittura del TIVGD
049400110617      **?__________________________________________________________________ */
049500110617     C     VGD_WRITE     Begsr
049600110617      *
049700110405     c                   clear                   tivgd000
049800110405     c                   eval      vgddta = %TrimR(Segmento)
049900110617     c                   eval      vgdtip = TipoF
050000130521     c                   eval      vgdksu = CLIENTE_trs
050100130430     C                   eval      vgdprg = SAV_Progr
050200110405     c                   eval      vgdtsc = 'WW'
050300130429     c                   eval      vgdpgm = 'TRTCT97R'
050400110405     c                   eval      vgddat = udtymd
050500110405      *
050600110405     C                   WRITE     tivgd000
050700110405      *
050800110405     c                   endsr
050900110405      **?__________________________________________________________________ */
051000110405      *    Chiusura del TIVGD
051100110405      **?__________________________________________________________________ */
051200110405     C     VGD_CLOSE     Begsr
051300110405      *
051400110405     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
051500110405     C                   clear                   trul47ds
051600110405     C                   eval      d47opz  = 'F'
051700110617     C                   eval      d47tip  = TipoF
051800110405     C                   call      'TRUL47R'
051900110405     C                   parm                    trul47ds
052000110405      *
052100110405     c                   endsr
052200110617      **?__________________________________________________________________ */
052300110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
052400130415      **?__________________________________________________________________ */
052500110621     C     *pssr         BEGSR
052600110621     C
052700130429     C                   eval      T97ESITO = '1'
052800110621     C                   ENDSR     '*CANCL'
052900110621     C
053000130415      * ?------------------------------------------------------------------ */
053100130415      * CTRL caricamento schiere    PT                               -*
053200130415      * ?------------------------------------------------------------------ */
053300130415     C     Check_PT      begsr
053400130415     C*
053500130415     c* Controllo riempimento schiera
053600130415     c                   clear                   trul0sds
053700130415     c                   eval      i0sski='PT_key'
053800130415     c                   eval      i0sele=%elem(PT_key)
053900130415     c                   eval      i0spie=x
054000130415     c                   eval      i0sfile='EDTAB00F'
054100130415     c                   eval      i0ssif=knsif
054200130429     c                   eval      i0spgm='TRTCT97R'
054300130415     c                   move      kpjbu         SvKpjbu
054400130415     c                   clear                   kpjbu
054500130415     c                   movel     trul0sds      kpjbu
054600130415     c                   call      'TRUL0SR'
054700130415     c                   parm                    kpjba
054800130415     c                   eval      kpjbu = SvKpjbu
054900130415     C*
055000130415     C                   ENDSR
055100130415      * ?------------------------------------------------------------------ */
055200130429      *?    CARICA TABELLE SU SCHIERE con il SOLO record INTERESSATO
055300130429      *  anche se probabilmente non vengono utilizzate le informazioni delle
055400130429      *  tabelle, sono pronte per essere utilizzate su eventuali modifiche future
055500130415      * ?------------------------------------------------------------------ */
055600130429     C     CARICA_EDTAB  BEGSR
055700130415      *
055800130415      * Caricamento Tabella Partner esteri
055900130429     C                   Z-ADD     0             X                 5 0
056000130415     C                   MOVEL     'PT'          TABCOD
056100130429     C     TABCOD        setll     EDTAB01L
056200130429     C     TABCOD        READE     EDTAB01L                               30
056300130429     C                   DOW       not %EoF(EDTAB01L)
056400130429     C                   MOVEL     TABUNI        edidsPT
056500130416      *
056600130415      * carica solo quello che interessa uno .... due ....tre record al max.
056700130429     C                   IF        TABFLG = *BLANKS  and
056800130429     C                               cliente7n = §PTKSC
056900130429      *   Carica solo il PRIMO TROVATO
057000130415     C                   ADD       1             X
057100130415     C                   MOVEL     TABKEY        PT_key(X)
057200130415     C                   MOVEL     TABUNI        PT_uni(X)
057300130416     c                   LEAVE
057400130415     C                   END
057500130429      *
057600130415     C     TABCOD        READE     EDTAB01L                               30
057700130415     C                   END
057800130415      *
057900130416      *  se non ha trovato nemmeno un record sulla PT lo segnala
058000130416     C                   IF            X = 0
058100130416???? C                   EVAL      Indirizzi = CED_Bart
058200130429???? C                   EVAL      Oggetto ='Problemi ORM DOWNLOAD x GEL'
058300130429     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione GEL -
058400130416     C                             ORM EXPORT - messaggio -
058500130429     C                             NON TROVATA tabella PT controllare EDI -
058600130429     C                             su DOWNLOAD.'
058700130416     c                   exsr      Invio_Mail
058800130429     c                   eval      T97ESITO = '1'
058900130416     C                   END
059000130416      *
059100130415      *  se deve inviare la mail di alert x limite quasi raggiunto.
059200130415     c                   exsr      ChecK_PT
059300130415      *
059400130415      * salva quanti Codici UNB ha caricato
059500130415     C                   MOVEL     'PU'          TABCOD
059600130416     C     KediTAB       CHAIN     EDTAB01L
059700130416     C                   if        %Found(EDTAB01L) and
059800130416     C                             TABFLG = *BLANKS
059900130416     C                   MOVEL     TABUNI        edidsPU
060000130418     c                   movel     §pucompat     Compatta_80       1
060100130415     C                   MOVEL     TABKEY        PU_key
060200130415     C                   Z-ADD     1             X
060300130415     C     PU_key        LOOKUP    PT_key(X)                              32
060400130415     C   32              MOVEL     TABUNI        PU_uni(X)
060500130415     C                   END
060600130415      *
060700130415      * Prosegue tab.PT e PU
060800130415     C                   MOVEL     'PV'          TABCOD
060900130416     C     KediTAB       CHAIN     EDTAB01L
061000130416     C                   if        %Found(EDTAB01L) and
061100130416     C                             TABFLG = *BLANKS
061200130416     C                   MOVEL     TABUNI        edidsPV
061300130415     C                   MOVEL     TABKEY        PV_key
061400130415     C                   Z-ADD     1             X
061500130415     C     PV_key        LOOKUP    PT_key(X)                              32
061600130415     C   32              MOVEL     TABUNI        PV_uni(X)
061700130416      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
061800130416      *  ricevuti:
061900130416     c                   movel     §PVema        mail_ind         44
062000130416      * ?imposta gli indirizzi mail se interni a BRT o esterni
062100130416     c                   if        mail_ind <> *blank
062200130416     c                   exsr      imp_ADDR_mail
062300130416     c                   end
062400130416      *
062500130416     C                   END
062600130415      *
062700130415      * Prosegue tab.PT e PU e PV
062800130415     C                   MOVEL     'PZ'          TABCOD
062900130416     C     KediTAB       CHAIN     EDTAB01L
063000130416     C                   if        %Found(EDTAB01L) and
063100130416     C                             TABFLG = *BLANKS
063200130416     C                   MOVEL     TABUNI        edidsPZ
063300130415     C                   MOVEL     TABKEY        PZ_key
063400130415     C                   Z-ADD     1             X
063500130415     C     PZ_key        LOOKUP    PT_key(X)                              32
063600130415     C   32              MOVEL     TABUNI        PZ_uni(X)
063700130521      **
063800130521      * se richiesta la trasmissione ad un codice UNIFICANTE PARTICOLARE
063900130521      **   deve sovrapporsi al codice cliente Generico legato all'UNB
064000130521     c                   if                  §puXXX = 'S' and
064100130521     c                                §PZFTPCOD <> *zeros and
064200130521     c                                §PZFTPCOD <> *blank
064300130521     c                   move      *all'0'       CLIENTE_trs
064400130521     c                   move      §PZFTPCOD     CLIENTE_trs
064500130521     c                   end
064600130521      *
064700130415     C                   END
064800130523      *
064900130523      * Se invece NON aveva questo tipo di invio con ALTRO CODICE imposta
065000130523      *  il codice cliente della TABELLA PT generale come codice trasmissione
065100130523     c                   if        CLIENTE_trs = *blank
065200130523     C                   MOVEL     CLIENTE8a     CLIENTE_trs
065300130523     c                   end
065400130415      *
065500130419      * Prosegue tab.PT e PU e PV e PO
065600130419     C                   MOVEL     'PO'          TABCOD
065700130419     C     KediTAB       CHAIN     EDTAB01L
065800130419     C                   if        %Found(EDTAB01L) and
065900130419     C                             TABFLG = *BLANKS
066000130419     C                   MOVEL     TABUNI        edidsPO
066100130419     C                   MOVEL     TABKEY        PO_key
066200130419     C                   Z-ADD     1             X
066300130419     C     PO_key        LOOKUP    PT_key(X)                              32
066400130419     C   32              MOVEL     TABUNI        PO_uni(X)
066500130419     C                   END
066600130415      *
066700130415     C                   ENDSR
066800130416     c*==================================================================*
066900130416      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
067000130416     c*==================================================================*
067100130416     c     Imp_ADDR_mail begsr
067200130416      *
067300130416     c                   clear                   Indirizzi
067400130416      *
067500130416      *  Lunghezza indirizzi presenti
067600130416     c                   eval      Lunghezza_Indirizzo =
067700130416     c                                       %Len(%TrimR(Mail_Ind))
067800130416     c                   z-add     1             al_char           3 0
067900130416     c                   z-add     1             dal_char          3 0
068000130416     c                   z-add     1             tot_char          3 0
068100130416      *
068200130416     c     successivo    tag
068300130416      *
068400130416      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
068500130416      *   e aggiunge il dominio Brt + il (;) separatore
068600130416     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
068700130416     c                   eval      tot_char = al_char - dal_char
068800130416     c                   z-add     dal_char      dalCarattere
068900130416     c                   z-add     tot_char      xTOTcaratteri
069000130416      *
069100130416     c                   clear                   Indir_BRT        40
069200130416     c                   clear                   Indir_Parz       20
069300130416     c                   eval      Indir_Parz =
069400130416     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
069500130416      *
069600130416     c                   eval      Indir_BRT    = %Trim(Indir_Parz) +
069700130416     c                             %Trim(bart_it)
069800130416     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_BRT
069900130416      *
070000130416     c                   if        al_char = Lunghezza_Indirizzo
070100130416     c                   goto      fine_indmail
070200130416     c                   else
070300130416     c                   eval      dal_char = ( al_char + 1 )
070400130416     c                   goto      successivo
070500130416     c                   end
070600130416      *
070700130416     C     fine_indmail  TAG
070800130416      *
070900130416     C                   ENDSR
071000130416     c*==================================================================*
071100130416      * Manda un Msg x E-mail
071200130416     c*==================================================================*
071300130416     c     Invio_Mail    begsr
071400130416      *
071500130416     C*   Alert_mail : invio Mail a CED@Brt.it                  *
071600130416     C* Inizializzo variabili
071700130416     C                   movel     *blanks       wrkEml          253
071800130416     C                   movel     *blanks       wrkMsg         5000
071900130416     C                   movel     *blanks       wrkOgg           44
072000130416      *
072100130416     C* Valorizzo i campi della e-m@ail - indirizzo
072200130416     C*******            eval      wrkEml = 'Andrea.Bertocchi@Brt.it'
072300130416     C                   eval      wrkEml = Indirizzi
072400130416     C                   eval      wrkOgg = Oggetto
072500130416     C                   eval      wrkMsg = Messaggio
072600130416      ** *****
072700130416     C                   call(e)   'TRTCT00R2'
072800130416     C                   parm                    wrkEml
072900130416     C                   parm                    wrkOgg
073000130416     C                   parm                    wrkMsg
073100130416     C*
073200130416     C                   ENDSR
073300130429      * ?------------------------------------------------------------------ */
073400130429     C* Solo la prima volta  aprendo in RT
073500130415      * ?------------------------------------------------------------------ */
073600130429     C     *inzsr        begSR
073700130429     C*
073800130429     C                   ENDSR
073900130429      * ?------------------------------------------------------------------ */
