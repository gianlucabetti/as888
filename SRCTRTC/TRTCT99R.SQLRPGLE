000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200050707     H DECEDIT('0.') DATEDIT(*YMD.)
000300130415     H* TRTCT99R         ---------------------------------------------------*
000400110519      **?__________________________________________________________________ */
000500110519     H*                                                                     *
000600130415     H* Scrittura del IFTMBP da O.R.M.    x Partner  (EXPORT)               *
000700130415     H*  versione IFCSUM D99B
000800050927      **?__________________________________________________________________ */
000900130415     FfnORM01L  IF   E           K DISK
001000130418     FedTAB01L  IF   E           K DISK
001100130418     FTABEL00F  IF   E           K DISK
001200130502     FTnTBE01L  IF   E           K DISK
001300130502     FAzORG01L  IF   E           K DISK
001400110519      *
001500080424     ftivgd00f  if a E             DISK    commit
001600130419     fEdiStd80F if   E           k DISK    usropn
001700110519      *
001800110617     Ftivir02L  IF   E           K DISK
001900110617     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
002000110617     F                                     PREFIX(f_) usropn
002100110519      **?__________________________________________________________________ */
002200130415      **  Elenco DS di tutti i Segmenti da tradurre
002300130415      **?------------------------------------------------------------------ */
002400130415???? D edS00BGM      E DS
002500130415???? D edS00CNI      E DS
002600130415???? D edS00COM      E DS
002700130415???? D edS00CTA      E DS
002800130415???? D edS00DTM      E DS
002900130415???? D edS00FTX      E DS
003000130415???? D  D05                   16    365    DIM(5)
003100130415???? D edS00GID      E DS
003200130415???? D edS00LOC      E DS
003300130415???? D edS00MEA      E DS
003400130415???? D edS00NAD      E DS
003500130415???? D edS00RFF      E DS
003600130415???? D edS00TDT      E DS
003700130415???? D edS00TSR      E DS
003800130415???? D edS00UNA      E DS
003900130415???? D edS00UNB      E DS
004000130415???? D edS00UNH      E DS
004100130415???? D edS00UNT      E DS
004200130415???? D edS00UNZ      E DS
004300130415      **?************************************************************************
004400130520???? D trtctORMds    E DS                  prefix(T99:3)
004500130502???? D  Does         E DS
004600130502???? D fnlv55ds      E DS
004700130419      **?************************************************************************
004800130415     D EDIDSPT       E DS
004900130415     D EDIDSPU       E DS
005000130415     D EDIDSPV       E DS
005100130415     D EDIDSPZ       E DS
005200130419     D EDIDSPO       E DS
005300130415     D EDIDSCL       E DS
005400130415     D EDIDSSS       E DS
005500130415     D EDIDSTC       E DS
005600130415      **?************************************************************************
005700130415     D KPJBA         E DS
005800130415     D     svkpjbu     s                   like(kpjbu)
005900130502      **
006000130502     D WLBDA8          DS
006100130502     D  G02DAT                 1      8  0
006200130502     D  G02INV                 9     16  0
006300130502     D  G02ERR                17     17
006400130502     D  G02TGI                18     22  0
006500130415      **?************************************************************************
006600130415      **  Definizione Qualificatori
006700130415      **?************************************************************************
006800130415???? D Tipo_Messaggio...
006900130415???? D                 s              3    INZ('335')
007000130415???? D Riferimento_Ordine...
007100130415???? D                 s              3    INZ('CR ')
007200130415???? D Riferimento_Destinatario...
007300130415???? D                 s              3    INZ('ANT')
007400130415???? D Riferimento_Cliente_FF...
007500130415???? D                 s              3    INZ('FF ')
007600130415???? D Telephone_Number...
007700130415???? D                 s              3    INZ('TE ')
007800130415???? D TeleFax_Number...
007900130415???? D                 s              3    INZ('TX ')
008000130415???? D Volume...
008100130415???? D                 s              3    INZ('VOL')
008200130415???? D Weight...
008300130415???? D                 s              3    INZ('WT ')
008400130415???? D G_Weight...
008500130415???? D                 s              3    INZ('G  ')
008600130415???? D Gross_Weight...
008700130415???? D                 s              3    INZ('AAG')
008800130415???? D Gross_Weight_E...
008900130415???? D                 s              3    INZ('AAE')
009000130415???? D Gross_Weight_D...
009100130415???? D                 s              3    INZ('AAD')
009200130415???? D N_Weight...
009300130415???? D                 s              3    INZ('N  ')
009400130415???? D Net_Weight...
009500130415???? D                 s              3    INZ('AAF')
009600130415???? D Kilograms...
009700130415???? D                 s              3    INZ('KGM')
009800130415???? D Grams...
009900130415???? D                 s              3    INZ('163')
010000130415???? D Meters...
010100130415???? D                 s              3    INZ('MTR')
010200130415???? D Cubic_Meters...
010300130415???? D                 s              3    INZ('MTQ')
010400130415???? D Data_senza_ora...
010500130415???? D                 s              3    INZ('102')
010600130415???? D Data_con_ora...
010700130415???? D                 s              3    INZ('203')
010800130415???? D Data_con_i_secondi...
010900130415???? D                 s              3    INZ('204')
011000130415     D controlla_COD_FTX...
011100130415???? D                 s              3
011200130415     d  Natura_Merce...
011300130415     d                 s              3    inz('AAA')
011400130415      **?------------------------------------------------------------------ */
011500130415     d UNB_diWork      s                   like(UNB0004)
011600130415     d  UNB_Mittente   s                   like(UNB0004)
011700130415     d BGM_Id_Testata  s             35A   inz(' ')
011800130415     d  RFF_RifRitiro  s             35A
011900130415     d UNB_parziale    s              1    inz('N')
012000130415     d UNB_Generico    s             31    inz(' ')
012100130415     d UNB_NrTrasmiss  s             14    inz(' ')
012200130415     d UNZ_NrTrasmiss  s             14    inz(' ')
012300130415     d UNH_Rifer_Msg   s             14    inz(' ')
012400130415      *
012500130415     D TRUL0SDS      E DS
012600130415      *
012700130415      * Tabella partner
012800130415     D PT_key          S             35    DIM(200)
012900130415     D PT_Parz         S             35    DIM(%elem(PT_key))
013000130415     D PT_ZZ           S              4    DIM(%elem(PT_key))
013100130415     D PT_uni          S             90    DIM(%elem(PT_key))
013200130415     D PU_uni          S             90    DIM(%elem(PT_key))
013300130415     D PV_uni          S             90    DIM(%elem(PT_key))
013400130415     D PZ_uni          S             90    DIM(%elem(PT_key))
013500130419     D PO_uni          S             90    DIM(%elem(PT_key))
013600130415      *
013700130415     D Nr_PT           S              3  0 INZ
013800130415     D PT_key_savXX    S              3  0 INZ
013900130415     D  PT_poe         s                   LIKE(§PTlnp)
014000130415     D  PT_cor         s                   LIKE(§PTksc)
014100130415     D  PT_trovata     s              1    inz('N')
014200130415     D  PT_con_piu_Nazioni...
014300130415     d                 s              1
014400130415     D PU_key          S             35
014500130415     D  PU_TipoServ    s              2a
014600130415     D  PU_SpecServ    s              2a
014700130415     D PV_key          S             35
014800130415     D PZ_key          S             35
014900130419     D PO_key          S             35
015000130415      *
015100130415     d  GID_aTotale    s              1A
015200130415     d  GID_ColliInAdd...
015300130415     d                 s                   like(gid722420)
015400130415      * ?================================================================== */
015500130415      * Tabella codici nazioni
015600130415     D Ds15          e DS
015700130415     D  Naz03          s              3    DIM(999)
015800130415     D  Iso03          s              3    DIM(999)
015900130415     D  Cpf03          S              2  0 DIM(999)
016000130415      * ?================================================================== */
016100130415     D Dorm01        e DS
016200130503     d prima_volta     s              1    inz(' ')
016300130415      *
016400130415      * Tabella CL --> codici clienti - tariffa
016500130415     D Cod_Tb_CL       S             35    DIM(999)
016600130415     D Des_Tb_CL       S             90    DIM(999)
016700130415      *
016800130415      * Priorità trasporto
016900130415     D Key_TipServ     S             35    DIM(100)
017000130415     D TipoServizio    S              1    DIM(100)
017100130415      *
017200130415      * Decodifiche servizi speciali
017300130415     D SpecialServ     S             35    DIM(100)
017400130415     D Key_ServSpec    S              3    DIM(100)
017500130415     D UNI_ServSpec    S             90    DIM(100)
017600130415      *
017700130415      * Schiere x routine di conversione CAP
017800130415     D CAP5            S              1    DIM(5)
017900130415     D CAP9            S              1    DIM(9)
018000130415     D CAPM            S              1    DIM(9)
018100130415      *
018200130415      * Schiera per memorizzazione FTX ricevuti
018300130415     D QUA             S              3    DIM(100)
018400130415     D FTX             S             70    DIM(100)
018500130415      *
018600130415      * Tabelle capiconto
018700130415      * Schiere x località
018800130415     D LOD             S              1    DIM(35)
018900130502      **?------------------------------------------------------------------ */
019000130502      *
019100130502     D SAV_Progr       S                   like(vgdPRG)
019200130502     D SAV_Cliente     S                   like(T99Cliente)
019300130502      * ?================================================================== */
019400130416      *  Invio E-mail
019500130416     D Indirizzi       s            253
019600130416     D Oggetto         s             44
019700130416     D Messaggio       s           5000
019800130416     d   DSmail        ds
019900130416     D Mail_Ind                      44
020000130416     d   IND_char                     1    dim(44)
020100130416      *
020200130416     D Lunghezza_Indirizzo...
020300130416     D                 s              5u 0
020400130416     D dalCarattere    s              5u 0
020500130416     D xTOTcaratteri   s              5u 0
020600130416      *
020700130416     d bart_it         C                   CONST('@Brt.it;')
020800130416     d CED_Bart        C                   CONST('CED@Brt.it;')
020900130415      *---------------------------------------------------------------------*
021000130521     D TipoF           C                   CONST('EM')
021100110617     D dwlprg          s             10    INZ(*all'0')
021200110617     D wrkprg          s              8  0 INZ(*zeros)
021300060814     d trul47ds      e ds
021400060814     D W0140           S             14  0 inz
021500110316     D Segmento        s           2048
021600110525     D Segmento1950    s           1950
021700110316     D Conta_righe     s              5i 0
021800130503     D Conta_UNH_UNT   s              4s 0
021900130503     D da_UNH_a_UNT    s              5i 0
022000111018     d lunghezza_campo...
022100130419     d                 s              5i 0
022200110617      *---------------------------------------------------------------------*
022300110617     D Var_ISV         s                   like(Virfi1)
022400110316     D*---------------------------------------------------------------------*
022500110520     D UNA_sgm         C                   CONST('UNA:+.? ''')
022600131008     D UNB_sgm         C                   CONST('UNB+UNOC:2+ITBAR005:ZZ+')
022700130503     D UNH_sgm         C                   CONST('UNH+')
022800110519     D BGM_sgm         C                   CONST('BGM+')
022900110518     D COM_sgm         C                   CONST('COM+')
023000110519     D CTA_sgm         C                   CONST('CTA+')
023100110518     D DTM_sgm         C                   CONST('DTM+')
023200110518     D FTX_sgm         C                   CONST('FTX+')
023300110519     D GID_sgm         C                   CONST('GID+')
023400130415     D LOC_sgm         C                   CONST('LOC+117')
023500130418     D MEA_sgm         C                   CONST('MEA+WT+AAD+KGM:')
023600110518     D NAD_sgm         C                   CONST('NAD+')
023700110518     D RFF_sgm         C                   CONST('RFF+')
023800130415     D TDT_sgm         C                   CONST('TDT+ZZZ')
023900130415     D TSR_sgm         C                   CONST('TSR+++3')
024000110518     D UNT_sgm         C                   CONST('UNT+')
024100130503     D UNZ_sgm         C                   CONST('UNZ+')
024200111018     D Punto_interrogativo...
024300111018     D                 C                   CONST('?')
024400111018     d un_blank        C                   CONST(' ')
024500050927      **?__________________________________________________________________ */
024600050704      *    main
024700110316      **?__________________________________________________________________ */
024800130415     c     *entry        plist
024900130415     C                   PARM                    kpjba
025000130520     C                   movel     kpjbu         trtctORMds
025100130416     c*
025200130419     c                   clear                   T99ESITO          1
025300130502     c*
025400130502     c                   IF        T99FUNZION <>'OPEN ' and
025500130502     c                             T99FUNZION <>'EXEC ' and
025600130502     c                             T99FUNZION <>'CLOSE'
025700130502???? C                   EVAL      Indirizzi = CED_Bart
025800130502???? C                   EVAL      Oggetto ='Problemi ORM DOWNLOAD EDI -
025900130502     C                             IFTMBP'
026000130502     C                   EVAL      Messaggio = 'ATTENZIONE : -
026100130502     C                             il TRTCT99R è stato richiamato in MODO -
026200130502     C                             anomalo. NON è stata passata la corrett-
026300130502     C                             a modalità: OPEN/EXEC/CLOSE per il Clie-
026400130502     c                             nte: ' + Cliente8a +  ' Controllare il -
026500130502     c                             programma Chiamante e corregerlo!'
026600130502     c                   exsr      Invio_Mail
026700130502     c                   eval      T99ESITO = '1'
026800130502     c                   SETON                                        LR
026900130502     c                   RETURN
027000130502     c                   end
027100130502      *
027200130502      *  Se deve INIZIARE passa l' OPEN
027300130502      * ?              /*--------------------------- */
027400130502     c                   IF        T99FUNZION = 'OPEN '
027500130502      * ?              /*--------------------------- */
027600130502     C                   TIME                    ORADAT           14 0
027700130502     C                   MOVEL     ORADAT        ORATR             6 0
027800130502     c                   move      *date         udtymd            8 0
027900130502     C                   TIME                    W0140
028000130502     C                   MOVE      W0140         W0080             8 0
028100130521     C                   MOVE      OraDat        G02DAT
028200130521     C                   MOVEL     *BLANK        G02ERR
028300130521     C                   CALL      'XSRDA8'
028400130521     C                   PARM                    WLBDA8
028500130521     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
028600130521      *
028700130502     C                   MOVE      T99CLIENTE    Cliente7n         7 0
028800130502     C                   MOVE      *zeros        Cliente8a         8
028900130502     C                   MOVE      T99CLIENTE    Cliente8a         8
029000130502     C                   z-add     T99CLIENTE    SAV_Cliente
029100130521     c                   move      *blanks       CLIENTE_trs       8
029200130521      *
029300130502      * apre il TIVGD
029400130502     C                   EXSR      vgd_OPEN
029500130502      * ?              /*--------------------------- */
029600130502     c                   exsr      CARICA_EDTAB
029700130502      * ?              /*--------------------------- */
029800130503     c                   if        Compatta_80 = 'S'
029900130503     C/EXEC SQL
030000130503     C+ DELETE   FROM EDISTD80F
030100130503     C/END-EXEC
030200130503     c                   end
030300130502      **
030400130502      * quindi chiude e conferma la scrittura del TIVGD
030500130502      * ?              /*--------------------------- */
030600130502     c                   elseIF    T99FUNZION = 'CLOSE'
030700130502      * ?              /*--------------------------- */
030800130503      *** chiude il messaggio
030900130503     c                   exsr      segm_UNZ
031000130503      ***
031100130502     c                   commit
031200130502     C                   EXSR      vgd_CLOSE
031300130502     c                   seton                                        LR
031400130502     c                   RETURN
031500130502      **
031600130502      * ?              /*--------------------------- */
031700130502     c                   End
031800130502      * ?              /*--------------------------- */
031900130416     c*
032000130416     c     kediTAB       klist
032100130416     C                   kfld                    tabCOD
032200130416     C                   kfld                    tabKEY
032300130418     c*
032400130418     c     kTABEL        klist
032500130418     C                   kfld                    tblkut
032600130418     C                   kfld                    tblcod
032700130502     c*
032800130502     c     kTnTBE        klist
032900130502     C                   kfld                    tbeCOD
033000130502     C                   kfld                    tbeKE1
033100130502     C                   kfld                    tbeKE2
033200130415     c*
033300130415     c     kORM          klist
033400130419     C                   kfld                    T99ORMPOE
033500130419     C                   kfld                    T99ORMNSR
033600130419     C                   kfld                    T99ORMNOR
033700130419     C                   kfld                    T99ORMNRV
033800130415     c*
033900110617     c     kvir02        klist
034000110617     C                   kfld                    virKSC
034100110617     C                   kfld                    virTIP
034200130502      **
034300130502     c                   IF        T99FUNZION = 'EXEC '
034400130502      **  se ha avuto dei problemi !!!!
034500130502     c                              and
034600130502     c                             T99ESITO <>'1'                                   x errori
034700130502      *
034800130415     c* Aggancia l'ORM per scrivere il messaggio
034900130415     c     kORM          chain     fnORM01l
035000130415     c*
035100130415     C                   if         %Found(FnORM01L)
035200130415     c                   exsr      LOAD_SEGMENTS
035300130416     c                   else
035400130416      *
035500130416      *    non ha trovato l'ORM
035600130419     c                   eval      T99ESITO = '1'                                  x errori
035700130416      *
035800050707      *
035900130416     c                   end
036000130416      *
036100130416     c                   end
036200130416      *
036300130502      *   chiude in RT
036400130502     c                   seton                                        RT
036500130415      **?------------------------------------------------------------------ */
036600130415      **    IMPOSTA I CAMPI DELL'ORM NEI SEGMENTI DELL' Edi Standard
036700130415      **?------------------------------------------------------------------ */
036800130415     C     LOAD_SEGMENTS Begsr
036900130415      *
037000130503      *   La SuperTestata la fa solo la prima volta e Basta poi al prossimo msg
037100130503      *    dopo aver chiuso in LR
037200130503     c                   if        prima_volta = *blank
037300130415      *  _____________UNA
037400130415      *  _____________UNB
037500130415     c                   clear                   eds00UNB
037600131008     c                   eval        UNB0001   = 'UNOC'
037700130415     c                   eval        UNB0002   = 2
037800130415     c                   eval        UNB0004   = 'ITBAR005'
037900130415     c                   eval        UNB000720 = 'ZZ  '
038000130502     c                   eval        UNB0010   = %subst(PT_key(1):1:31)
038100130415     c                   eval        UNB000730 = PT_zz(1)
038200130415     c                   move      udtymd        UNB0017
038300130419     c                   movel     ORATR         UNB0019
038400130415     c                   eval        UNB0020   =
038500130419     c                                    %editc(T99ORMPOE:'X') +
038600130419     c                                    %editc(T99ORMNSR:'X') +
038700130419     c                                    %editc(T99ORMNOR:'X') +
038800130419     c                                    %editc(T99ORMNRV:'X')
038900130503      **  [scrive TIVGD]
039000130503     c                   exsr      segm_UNA
039100130503     c                   exsr      segm_UNB
039200130503     c                   eval      prima_volta = 'S'
039300130503     c                   end
039400130415      *  _____________UNH
039500130415     c                   clear                   eds00UNH
039600130415     c                   eval       UNH0065 = 'IFTMBP'
039700130415     c                   eval       UNH0052 = 'D'
039800130415     c                   eval       UNH0054 = '99B'
039900130415     c                   eval       UNH0051 = 'UN'
040000130415      *  _____________BGM
040100130415     c                   clear                   eds00BGM
040200130415     c                   eval       BGM1001 = Tipo_Messaggio
040300130415     c                   eval       BGM1004 =
040400130419     c                                    %editc(T99ORMPOE:'X') +
040500130419     c                                    %editc(T99ORMNSR:'X') +
040600130419     c                                    %editc(T99ORMNOR:'X') +
040700130419     c                                    %editc(T99ORMNRV:'X')
040800130415     c                   eval       BGM1225 = '9'
040900130415      *
041000130503      **  [scrive TIVGD]
041100130415     c                   exsr      segm_UNH
041200130415     c                   exsr      segm_BGM
041300130415      *  _____________DTM
041400130415     c                   clear                   eds00DTM
041500130415     c                   eval         DTM2005 = '137'
041600130430     c                   eval         DTM2380 = %editc(ormDAO:'X') +
041700130430     c                                          %editc(ormOAO:'X')
041800130415     c                   eval         DTM2379 = '204'
041900130415      *
042000130415      **  [scrive TIVGD]
042100130415     c                   exsr      segm_DTM
042200130415     c                   exsr      segm_TDT
042300130415     c                   exsr      segm_TSR
042400130415     c                   exsr      segm_LOC
042500130415      *  _____________DTM
042600130415     c                   clear                   eds00DTM
042700130415     c                   eval         DTM2005 = '200'
042800130419     c                   eval         DTM2380 = %editc(ormDAR:'X') +
042900130419     c                                          %editc(ormORR:'X')
043000130415     c                   eval         DTM2379 = '204'
043100130415      **  [scrive TIVGD]
043200130415     c                   exsr      segm_DTM
043300130415      *  _____________NAD
043400130415     c                   clear                   eds00NAD
043500130415     c                   eval       NAD3035  = 'HI'
043600130418      *  Ordinante                                      PARTY IDENTIFIER
043700130502      *  ATTENZIONE QUESTA DEVE ESSERE LA FILIALE BARTOLINI DEL ormPOR
043800130419     C                   if        ormcor  = *all'0'
043900130419     C                   eval          NAD3039 = *blank
044000130419     c                   else
044100130521     c                   movel     ORMCOR        NAD3039
044200130419     c                   end
044300130502      *** aggancia la tabella "OES" tramite il codice di RITIRO ormPOR
044400130502      **
044500130502     c                   eval      tbeCOD ='OES'
044600130502     c                   eval      tbeKE1 ='0'+ %editc(ormPOR:'X')
044700130502     c                   eval      tbeKE2 = *blank
044800130502      *
044900130502     c     kTnTbe        chain     tntbe01L
045000130502     c                   if        %Found(tntbe01L)
045100130502      *
045200130502     c                   eval      dOES = tbeUNI
045300130502      *                                                 PARTY NAME
045400130502     c                   eval       NAD30361= d§oesdes
045500130502      *
045600130502      * PRENDO TERMINAL ARRIVO   LINEA DI ARRIVO
045700130502     C                   CLEAR                   FNLV55DS
045800130502     C                   MOVEL     'A'           D55TPT
045900130502     C                   MOVEL     WOGGI         D55DRF
046000130502     C                   MOVEL     ormPOR        D55LIN
046100130502     C                   CALL      'FNLV55R'
046200130502     C                   PARM                    FNLV55DS
046300130502      *
046400130502      * con il Terminal di arrivo prende l'indirizzo
046500130502     C     D55TFA        chain     azORG01l
046600130502     c                   if        %Found(AzOrg01L)
046700130502      *                                                 STREET AND NR.OR POST OFF
046800130502     c                   eval       NAD30421= orgIND
046900130502      *                                                 CITY NAME
047000130502     c                   eval       NAD3164 = orgLOC
047100130502      *                                                 POSTAL IDENTIFICATION CODE
047200130502     c                   movel     orgCPF        NAD3251
047300130502      *                                                 COUNTRY IDENTIFIER
047400130502     c                   eval       NAD3207 = 'IT'
047500130502      *
047600130502      **  [scrive TIVGD]
047700130502     c                   exsr      segm_NAD
047800130502      *
047900130502      *  _____________CTA
048000130502     c                   clear                   eds00CTA
048100130502     c                   eval        CTA3139  = 'IC'
048200130502     c                   if         D§OESNOM <> *blank
048300130502     c                   eval        CTA3412  = D§OESNOM
048400130502     c                   else
048500130502     c                   eval        CTA3412  = '.'
048600130502     c                   end
048700130502      **  [scrive TIVGD]
048800130502     c                   exsr      segm_CTA
048900130502      *  _____________COM
049000130502     c                   clear                   eds00COM
049100130502     c                   if         D§OESTEL <> *blank
049200130502     c                   eval        COM3148  = D§OESTEL
049300130502     c                   else
049400130502     c                   eval        COM3148  = '.'
049500130502     c                   end
049600130502     c                   eval        COM3155  = 'TE'
049700130502      **  [scrive TIVGD]
049800130502     c                   exsr      segm_COM
049900130418      *  _____________RFF
050000130418     c                   clear                   eds00RFF
050100130418     c                   eval       RFF1153 = 'CR'
050200130418     c                   eval       RFF1154 =
050300130419     c                                    %editc(T99ORMPOE:'X') +
050400130419     c                                    %editc(T99ORMNSR:'X') +
050500130419     c                                    %editc(T99ORMNOR:'X') +
050600130419     c                                    %editc(T99ORMNRV:'X')
050700130418      **  [scrive TIVGD]
050800130418     c                   exsr      segm_RFF
050900130502     c                   end
051000130502     c                   end
051100130502      *
051200130418      *  _____________NAD
051300130418     c                   clear                   eds00NAD
051400130418     c                   eval       NAD3035  = 'CZ'
051500130418      *  Dove RITIRARE                                  PARTY IDENTIFIER
051600130419     C                   if        ormcra  = *all'0'
051700130419     C                   eval         NAD3039  = *blank
051800130419     c                   else
051900130521     c                   movel     ORMCRA        NAD3039
052000130419     c                   end
052100130418      *                                                 PARTY NAME
052200130418     c                   eval       NAD30361= ORMRSR
052300130418      *                                                 STREET AND NR.OR POST OFF
052400130418     c                   eval       NAD30421= ORMINR
052500130418      *                                                 CITY NAME
052600130418     c                   eval       NAD3164 = ORMLOR
052700130418      *                                                 POSTAL IDENTIFICATION CODE
052800130418     c                   eval       NAD3251 = ORMCAR
052900130418      *                                                 COUNTRY IDENTIFIER
053000130419     c                   if        ORMNAR = *blank
053100130419     c                   movel     'IT'          ORMnar
053200130419     c                   end
053300130418     c                   eval       NAD3207 = ORMNAR
053400130418      **  [scrive TIVGD]
053500130418     c                   exsr      segm_NAD
053600130418      *  _____________CTA
053700130418     c                   clear                   eds00CTA
053800130418     c                   eval        CTA3139  = 'IC'
053900130502     c                   if        ORMRER <> *blank
054000130418     c                   eval        CTA3412  = ORMRER
054100130502     c                   else
054200130502     c                   eval        CTA3412  = '.'
054300130502     c                   end
054400130418      **  [scrive TIVGD]
054500130418     c                   exsr      segm_CTA
054600130418      *  _____________COM
054700130418     c                   clear                   eds00COM
054800130418     c                   if        ORMTER <> *blank
054900130418     c                   eval        COM3148  = ORMTER
055000130418     c                   else
055100130418     c                   eval        COM3148  = '.'
055200130418     c                   end
055300130418     c                   eval        COM3155  = 'TE'
055400130418      **  [scrive TIVGD]
055500130418     c                   exsr      segm_COM
055600130418      *  _____________RFF
055700130418     c                   clear                   eds00RFF
055800130418     c                   eval       RFF1153 = 'CR'
055900130502     c                   eval       RFF1154 =
056000130502     c                                    %editc(T99ORMPOE:'X') +
056100130502     c                                    %editc(T99ORMNSR:'X') +
056200130502     c                                    %editc(T99ORMNOR:'X') +
056300130502     c                                    %editc(T99ORMNRV:'X')
056400130418      **  [scrive TIVGD]
056500130418     c                   exsr      segm_RFF
056600130418      *  _____________NAD
056700130418     c                   clear                   eds00NAD
056800130418     c                   eval       NAD3035  = 'CN'
056900130430      *  Dove CONSEGNARE                                PARTY IDENTIFIER
057000130419     C                   if        ormcrc  = *all'0'
057100130419     C                   eval         NAD3039  = *blank
057200130419     c                   else
057300130521     c                   movel     ORMCRC        NAD3039
057400130419     c                   end
057500130418      *                                                 PARTY NAME
057600130418     c                   eval       NAD30361= ORMRSC
057700130418      *                                                 STREET AND NR.OR POST OFF
057800130418     c                   eval       NAD30421= ORMINC
057900130418      *                                                 CITY NAME
058000130418     c                   eval       NAD3164 = ORMLOC
058100130418      *                                                 POSTAL IDENTIFICATION CODE
058200130418     c                   eval       NAD3251 = ORMCAC
058300130418      *                                                 COUNTRY IDENTIFIER
058400130419     c                   if        ORMNAC = *blank
058500130419     c                   movel     'IT'          ORMnac
058600130419     c                   end
058700130418     c                   eval       NAD3207 = ORMNAC
058800130418      **  [scrive TIVGD]
058900130418     c                   exsr      segm_NAD
059000130418      *  _____________RFF
059100130418     c                   clear                   eds00RFF
059200130418     c                   eval       RFF1153 = 'CR'
059300130418     c                   eval       RFF1154 =
059400130419     c                                    %editc(T99ORMPOE:'X') +
059500130419     c                                    %editc(T99ORMNSR:'X') +
059600130419     c                                    %editc(T99ORMNOR:'X') +
059700130419     c                                    %editc(T99ORMNRV:'X')
059800130418      **  [scrive TIVGD]
059900130418     c                   exsr      segm_RFF
060000130418      *  _____________RFF
060100130418     c                   clear                   eds00RFF
060200130418     c                   eval       RFF1153 = 'ANT'
060300130418     c                   eval       RFF1154 =
060400130419     c                                    %editc(T99ORMPOE:'X') +
060500130419     c                                    %editc(T99ORMNSR:'X') +
060600130419     c                                    %editc(T99ORMNOR:'X') +
060700130419     c                                    %editc(T99ORMNRV:'X')
060800130418      **  [scrive TIVGD]
060900130418     c                   exsr      segm_RFF
061000130418      *  _____________FTX
061100130419     c                   if        ORMNAM <> *blank
061200131008      ***
061300131008      * nuova mappatura Ducros chiedono di scrivere un record --> TSR+++3'
061400131008      *  prima della Natura merce (quando presente la Natura merce)
061500131008     c                   exsr      segm_TSR
061600131008      ***
061700130418     c                   clear                   eds00FTX
061800131008     c                   eval       FTX4451 = Natura_Merce
061900130418     c                   eval       FTX44401= ORMNAM
062000130418      **  [scrive TIVGD]
062100130415     c                   exsr      segm_FTX
062200130419     c                   end
062300130418      *  _____________GID
062400130418     c                   clear                   eds00GID
062500130418     c                   eval       GID1496   = 99999
062600130828      *
062700130828      * come unità sono i colli + i bancali
062800130828     c                   if        ormNCL + ormBNC >  0
062900130419     c                   eval       GID706520 = 'PK'
063000130828     c                   eval       GID722420 = ORMNCL + ORMBNC
063100130828     c                   end
063200130828      * Bancali
063300130828     c                   if        ormBNC >  0
063400130828     c                   eval       GID706530 = 'EUR'
063500130828     c                   eval       GID722430 = ORMBNC
063600130419     c                   end
063700130418      **  [scrive TIVGD]
063800130418     c                   exsr      segm_GID
063900130418      *  _____________FTX
064000130419     c                   if        ORMNO1 <> *blank
064100130418     c                   clear                   eds00FTX
064200130418     c                   eval       FTX4451 = 'ACB'
064300130418     c                   eval       FTX44401= ORMNO1
064400130418      **  [scrive TIVGD]
064500130415     c                   exsr      segm_FTX
064600130419     c                   end
064700130418      *  _____________FTX
064800130419     c                   if        ORMNO2 <> *blank
064900130418     c                   clear                   eds00FTX
065000130418     c                   eval       FTX4451 = 'ACB'
065100130418     c                   eval       FTX44401= ORMNO2
065200130418      **  [scrive TIVGD]
065300130418     c                   exsr      segm_FTX
065400130419     c                   end
065500130502      *  _____________FTX
065600130502     c                   if        ORMrfa <> *blank
065700130502     c                   clear                   eds00FTX
065800130502     c                   eval       FTX4451 = 'ACB'
065900130502     c                   eval       FTX44401= ORMrfa
066000130502      **  [scrive TIVGD]
066100130502     c                   exsr      segm_FTX
066200130502     c                   end
066300130418      *  _____________MEA
066400130418     c                   clear                   eds00MEA
066500130419     c                   z-add     ORMPKG        MEA6314
066600130418      **  [scrive TIVGD]
066700130415     c                   exsr      segm_MEA
066800130415
066900130418      **  [scrive TIVGD]
067000130415     c                   exsr      segm_UNT
067100130415
067200130415      * conferma la scrittura del TIVGD
067300130415     c                   commit
067400130415      *
067500130415     C                   Endsr
067600110316      **?------------------------------------------------------------------ */
067700110316      **    Scrittura  del segmento UNA                Edi Standard
067800110316      **?------------------------------------------------------------------ */
067900110316     C     Segm_UNA      Begsr
068000110525      *
068100110525      *  Se deve compattare pulisce il file di lavoro in QTEMP EDISTD80F
068200110525     c                   if        compatta_80  ='S'
068300110525     C                   EXSR      CLEAR_STD80F
068400110525     c                   end
068500110525      *
068600110318     c                   eval         segmento = UNA_sgm
068700110518     C                   EXSR      TIVGD
068800110316      *
068900110316     C                   Endsr
069000110316      **?------------------------------------------------------------------ */
069100110316      **    Scrittura  del segmento UNB                Edi Standard
069200110316      **?------------------------------------------------------------------ */
069300110316     C     Segm_UNB      Begsr
069400110316      *
069500110318     c                   eval         segmento = UNB_sgm
069600130415     c                              + %Trim(UNB0010)
069700110519      *
069800110322      *  Aggiunge il qualificatore se c'è
069900130415     c                   if           UNB000730 <> *blank
070000110322     c                   eval         segmento = %Trim(segmento)
070100110316     c                              + ':'
070200130415     c                              + %Trim(UNB000730)
070300110322     c                   end
070400110519      *
070500110322     c                   eval         segmento = %Trim(segmento)
070600110316     c                              + '+'
070700130415     c                              + %Trim(%editc(UNB0017:'X'))
070800110316     c                              + ':'
070900130415     c                              + %Editc(UNB0019:'X')
071000110316     c                              + '+'
071100130415     c                              + %Trim(UNB0020)
071200110316
071300110518     C                   EXSR      TIVGD
071400110316      *
071500130503     c                   eval      Conta_UNH_UNT = 0
071600130503      *
071700110316     C                   Endsr
071800110316      **?------------------------------------------------------------------ */
071900110316      **    Scrittura  del segmento UNH                Edi Standard
072000110316      **?------------------------------------------------------------------ */
072100110316     C     Segm_UNH      Begsr
072200110316      *
072300130503     c                   eval         da_UNH_a_UNT = 1
072400130503     c                   eval      Conta_UNH_UNT = Conta_UNH_UNT +1
072500130503     c                   eval      UNH0062 = %editc(Conta_UNH_UNT:'X')
072600130503      *
072700110318     c                   eval         segmento = UNH_sgm
072800130503     c                              + %Trim(UNH0062)
072900130503     c                              + '+'
073000130503     c                              + %Trim(UNH0065)
073100130503     c                              + ':'
073200130503     c                              + %Trim(UNH0052)
073300130503     c                              + ':'
073400130503     c                              + %Trim(UNH0054)
073500130503     c                              + ':'
073600130503     c                              + %Trim(UNH0051)
073700110316
073800110518     C                   EXSR      TIVGD
073900110316      *
074000110316     C                   Endsr
074100110316      **?------------------------------------------------------------------ */
074200110316      **    Scrittura  del segmento BGM                Edi Standard
074300110316      **?------------------------------------------------------------------ */
074400110316     C     Segm_BGM      Begsr
074500110316      *
074600110318     c                   eval         segmento = BGM_sgm
074700130415     c                              + %Trim(BGM1001)
074800110519     c                              + '+'
074900130415     c                              + %Trim(BGM1004)
075000110316     c                              + '+'
075100130415     c                              + %Trim(BGM1225)
075200110316
075300110518     C                   EXSR      TIVGD
075400110316      *
075500110316     C                   Endsr
075600130418      **?------------------------------------------------------------------ */
075700130418      **    Scrittura  del segmento DTM                Edi Standard
075800130418      **?------------------------------------------------------------------ */
075900130418     C     Segm_DTM      Begsr
076000130418      *
076100130418     c                   if         DTM2005 <> *blank
076200130418     c                   eval         segmento = DTM_sgm
076300130418     c                              + %Trim(DTM2005)
076400130418     c                              + ':'
076500130418     c                              + %Trim(DTM2380)
076600130418     c                              + ':'
076700130418     c                              + %Trim(DTM2379)
076800130418      *
076900130418     C                   EXSR      TIVGD
077000130418     c                   end
077100130418      *
077200130418     C                   Endsr
077300130418      **?------------------------------------------------------------------ */
077400130418      **    Scrittura  del segmento TDT                Edi Standard
077500130418      **?------------------------------------------------------------------ */
077600130418     C     Segm_TDT      Begsr
077700130418      ***
077800130418     c                   eval         segmento = TDT_sgm
077900130418      ***
078000130418     C                   EXSR      TIVGD
078100130418      *
078200130418     C                   Endsr
078300130418      **?------------------------------------------------------------------ */
078400130418      **    Scrittura  del segmento TSR                Edi Standard
078500130418      **?------------------------------------------------------------------ */
078600130418     C     Segm_TSR      Begsr
078700130418      *
078800130418     c                   eval         segmento = TSR_sgm
078900130419      *   se c'è il Fermo Deposito
079000130419     c                   if        ORMFFD = 'S'
079100130419     c                   eval         segmento = %trim(segmento)
079200130419     c                                + '+GWC'
079300130419     c                   end
079400130418      *
079500130418     C                   EXSR      TIVGD
079600130418      *
079700130418     C                   Endsr
079800130418      **?------------------------------------------------------------------ */
079900130418      **    Scrittura  del segmento LOC                Edi Standard
080000130418      **?------------------------------------------------------------------ */
080100130418     C     Segm_LOC      Begsr
080200130418      *
080300130418     c                   eval         segmento = LOC_sgm
080400130418      *
080500130418     C                   EXSR      TIVGD
080600130418      *
080700130418     C                   Endsr
080800130418      **?------------------------------------------------------------------ */
080900130418      **    Scrittura  del segmento NAD                Edi Standard
081000130418      **?------------------------------------------------------------------ */
081100130418     C     Segm_NAD      Begsr
081200130418      *
081300130418     c                   eval         segmento = NAD_sgm
081400130418      *
081500130418     c                   eval      campo_alfa = NAD3039
081600130418     c                   eval      lunghezza_campo=%len(NAD3039)
081700130418     c                   exsr      Char_Speciali
081800130418     c                   eval      NAD3039 = campo_alfa
081900130418      *
082000130418     c                   eval      campo_alfa = NAD30361
082100130418     c                   eval      lunghezza_campo=%len(NAD30361)
082200130418     c                   exsr      Char_Speciali
082300130418     c                   eval      NAD30361 = campo_alfa
082400130418      *
082500130418     c                   eval      campo_alfa = NAD30421
082600130418     c                   eval      lunghezza_campo=%len(NAD30421)
082700130418     c                   exsr      Char_Speciali
082800130418     c                   eval      NAD30421 = campo_alfa
082900130418      *
083000130418     c                   eval      campo_alfa = NAD3164
083100130418     c                   eval      lunghezza_campo=%len(NAD3164)
083200130418     c                   exsr      Char_Speciali
083300130418     c                   eval      NAD3164 = campo_alfa
083400130418      *
083500130418     c                   eval      campo_alfa = NAD3251
083600130418     c                   eval      lunghezza_campo=%len(NAD3251)
083700130418     c                   exsr      Char_Speciali
083800130418     c                   eval      NAD3251 = campo_alfa
083900130418      *
084000130418     c                   eval         segmento = %Trim(segmento)
084100130418     c                              + %Trim(NAD3035)
084200130418     c                              + '+'
084300130521     c*********                     + %Trim(NAD3039)
084400130418     c                              + '+'
084500130418     c                              + '+'
084600130418     c                              + %Trim(NAD30361)
084700130418     c                              + '+'
084800130418     c                              + %Trim(NAD30421)
084900130418     c                              + '+'
085000130418     c                              + %Trim(NAD3164)
085100130418     c                              + '+'
085200130418     c                              + '+'
085300130418     c                              + %Trim(NAD3251)
085400130418     c                              + '+'
085500130418     c                              + %Trim(NAD3207)
085600130418      *
085700130418     C                   EXSR      TIVGD
085800130418      *
085900130418     C                   Endsr
086000130418      **?------------------------------------------------------------------ */
086100130418      **    Scrittura  del segmento RFF                Edi Standard
086200130418      **?------------------------------------------------------------------ */
086300130418     C     Segm_RFF      Begsr
086400130418      *
086500130418     c                   if        RFF1153 <> *blank
086600130418     c                   eval         segmento = RFF_sgm
086700130418     c                              + %Trim(RFF1153)
086800130418     c                              + ':'
086900130418     c                              + %Trim(RFF1154)
087000130418      *
087100130418     C                   EXSR      TIVGD
087200130418     c                   end
087300130418      *
087400130418     C                   Endsr
087500130418      **?------------------------------------------------------------------ */
087600130418      **    Scrittura  del segmento CTA                Edi Standard
087700130418      **?------------------------------------------------------------------ */
087800130418     C     Segm_CTA      Begsr
087900130418      *
088000130418     c                   eval         segmento = CTA_sgm
088100130418      *
088200130418     c                   eval      campo_alfa = CTA3412
088300130418     c                   eval      lunghezza_campo=%len(CTA3412)
088400130418     c                   exsr      Char_Speciali
088500130418     c                   eval      CTA3412 = campo_alfa
088600130418      *
088700130418     c                   eval         segmento = %Trim(segmento)
088800130418     c                              + %Trim(CTA3139)
088900130418     c                              + '+:'
089000130418     c                              + %Trim(CTA3412)
089100130418
089200130418     C                   EXSR      TIVGD
089300130418      *
089400130418     C                   Endsr
089500130418      **?------------------------------------------------------------------ */
089600130418      **    Scrittura  del segmento COM                Edi Standard
089700130418      **?------------------------------------------------------------------ */
089800130418     C     Segm_COM      Begsr
089900130418      *
090000130418     c                   eval         segmento = COM_sgm
090100130418      *
090200130418     c                   eval      campo_alfa = COM3148
090300130418     c                   eval      lunghezza_campo=%len(COM3148)
090400130418     c                   exsr      Char_Speciali
090500130418     c                   eval      COM3148 = campo_alfa
090600130418      *
090700130418     c                   eval         segmento = %Trim(segmento)
090800130418     c                              + %Trim(COM3148)
090900130418      *
091000130418     c                   if        COM3155 <> ' '
091100130418     c                   eval         segmento = %Trim(segmento)
091200130418     c                              + ':'
091300130418     c                              + %Trim(COM3155)
091400130418     c                   end
091500130418
091600130418     C                   EXSR      TIVGD
091700130418      *
091800130418     C                   Endsr
091900110519      **?------------------------------------------------------------------ */
092000110519      **    Scrittura  del segmento FTX                Edi Standard
092100110519      **?------------------------------------------------------------------ */
092200110519     C     Segm_FTX      Begsr
092300110519      *
092400130418     c                   if         FTX4451 <> *blank
092500110524      *
092600130418     c                   eval      campo_alfa = FTX44401
092700130418     c                   eval      lunghezza_campo=%len(FTX44401)
092800110524     c                   exsr      Char_Speciali
092900130418     c                   eval      FTX44401= campo_alfa
093000110524      *
093100110519     c                   eval         segmento = FTX_sgm
093200130418     c                              + %Trim(FTX4451)
093300110519     c                              + '+++'
093400130418     c                              + %Trim(FTX44401)
093500110519      *
093600110519     C                   EXSR      TIVGD
093700110519     c                   end
093800110519      *
093900110519     C                   Endsr
094000130418      **?------------------------------------------------------------------ */
094100130418      **    Scrittura  del segmento GID                Edi Standard
094200130418      **?------------------------------------------------------------------ */
094300130418     C     Segm_GID      Begsr
094400130418      *
094500130418     c                   eval         segmento = GID_sgm
094600130418     c                              + %Trim(%EDITC(GID1496:'Z'))
094700130418     c                              + '+'
094800130418     c                              + %Trim(%EDITC(GID722420:'Z'))
094900130418     c                              + ':'
095000130418     c                              + %Trim(GID706520)
095100130828      * se ci sono i BANCALI
095200130828     c                   if         GID706530 <> *blank
095300130828     c                   eval         segmento = %trim(segmento)
095400130828     c                              + '+'
095500130828     c                              + %Trim(%EDITC(GID722430:'Z'))
095600130828     c                              + ':'
095700130828     c                              + %Trim(GID706530)
095800130828     C                   End
095900130418
096000130418     C                   EXSR      TIVGD
096100130418      *
096200130418     C                   Endsr
096300110316      **?------------------------------------------------------------------ */
096400110519      **    Scrittura  del segmento MEA                Edi Standard
096500110316      **?------------------------------------------------------------------ */
096600110519     C     Segm_MEA      Begsr
096700110520      *
096800130419     c                   clear                   campo18          18
096900130419     c                   eval       campo18 = %editc(MEA6314:'Z')
097000110316      *
097100110519     c                   eval         segmento = MEA_sgm
097200130419     c                              + %Trim(CAMPO18)
097300110316      **
097400110316     C                   EXSR      TIVGD
097500110316      *
097600110316     C                   Endsr
097700110316      **?------------------------------------------------------------------ */
097800110316      **    Scrittura  del segmento UNT                Edi Standard
097900110316      **?------------------------------------------------------------------ */
098000110316     C     Segm_UNT      Begsr
098100110316      *
098200110316     c                   eval         segmento = UNT_sgm
098300130503     c                              + %Trim(%EDITC(da_UNH_a_UNT:'Z'))
098400130503     c                              + '+' + UNH0062
098500110316
098600110316     C                   EXSR      TIVGD
098700110316      *
098800110316     C                   Endsr
098900110316      **?------------------------------------------------------------------ */
099000110316      **    Scrittura  del segmento UNZ                Edi Standard
099100110316      **?------------------------------------------------------------------ */
099200110316     C     Segm_UNZ      Begsr
099300110316      *
099400110316     c                   eval         segmento = UNZ_sgm
099500130503     c                              + %Trim(UNH0062)
099600130503     c                              + '+'
099700130503     c                              + %Trim(UNB0020)
099800110316
099900110316     C                   EXSR      TIVGD
100000110525      *
100100110525      ** a questo punto scarica quello che ha compattato sul TIVGD x il DOWNLOAD
100200110525     c                   if        compatta_80  = 'S'
100300110525     c                   exsr      Scarica_su_VGD
100400110525     c                   end
100500110316      *
100600110316     C                   Endsr
100700110316      **?------------------------------------------------------------------ */
100800110405      **    FINTO ROLLBACK
100900110316      **?------------------------------------------------------------------ */
101000110405     C     ROLL_BCK      Begsr
101100110316      *
101200110405      *  E' meglio non gestire un ROLLBACK FISICO sul TIVGD poichè il file
101300110405      *  è troppo delicato nella sua funzione di DOWNLOAD.
101400110405      *   Se NON SI DEVE scrivere nulla poichè non si è rilevato nessuno stato
101500110405      *   da inviare al cliente, al momento si esce dal pgm senza aver fatto nulla
101600110405      *   Altrimenti questa è la routine per gestire qualche azione da compiere.
101700110405      *
101800110405      *
101900110405     C                   Endsr
102000110520      **?------------------------------------------------------------------ */
102100110520      **    Caratteri particolari
102200110520      **?------------------------------------------------------------------ */
102300110520     C     Char_Speciali Begsr
102400110520      *
102500110520      *  Controlla su campi alfanumerici la presenza di caratteri Speciali
102600110520      *   dichiarati nei segmenti    (+:'?)
102700110520      *   [più, due punti, apostrofo, punto interrogativo]
102800110520      *  Sono i caratteri che servono anche alla struttura EDI e quindi
102900110520      *   considerati caratteri Funzionali che, qualora debbano essere
103000110520      *   utilizzati come semplici caratteri di testo, dovranno essere
103100110520      *   preceduti da un (?).
103200110520      *
103300110520     c                   movel(p)  campo_alfa    campo_test
103400110520     c                   clear                   campo_ritorno
103500110520      *
103600110520     C                   call      'TRTCT00R3'
103700110520     c                   parm                    campo_test      512
103800110520     c                   parm                    campo_ritorno   512
103900130419     c                   parm                    ESITO             1
104000130419     c                   EVAL      T99ESITO = ESITO
104100110520      *
104200110520     c                   movel(p)  campo_ritorno campo_alfa      512
104300111018      ******
104400111018      *  occorre fare attenzione se l'ultimo carattere è un (?) che precede
104500111018      *   uno dei carratteri speciali, questo può procurare problemi a chi riceve
104600111018      *    quindi si deve sostituire il (?) con un blank e togliere tutto ciò che
104700111018      *   sta sulla destra da lì in poi.
104800111018     c                   if        %subst(campo_alfa:lunghezza_campo-1:1) <>
104900111018     c                                     Punto_interrogativo   and
105000111018     c                             %subst(campo_alfa:lunghezza_campo:1)
105100111018     c                                 =   Punto_interrogativo
105200111018      *   Sostituisce il (?) nell'ultimo byte del campo se non aveva nel byte
105300111018      *    precedente un (?) e gli mette blank al posto del (?)
105400111018     c                   eval      %subst(campo_alfa:lunghezza_campo:1)
105500111018     c                                 =   un_blank
105600111018      * si è fatto ciò poichè altrimenti quando si va a comporre il segmento EDI
105700111018      *  il carattere funzionale (+ o : o ') potrebbero venire annullati dalla
105800111018      *  presenza di un (?) davanti creando problemi di traduzione.
105900111018     c                   end
106000110520      *
106100110520     C                   Endsr
106200110405      **?------------------------------------------------------------------ */
106300110405      **    Scrittura  del segmento                    Edi Standard
106400110405      **?------------------------------------------------------------------ */
106500110405     C     tivgd         Begsr
106600110519      *
106700130503      *  conta righe in un dettaglio
106800130503     c                   eval         da_UNH_a_UNT = 1 + da_UNH_a_UNT
106900130503      *
107000110519      *   chiude il segmento con l'apostrofo
107100110520     c                   if         Conta_righe > 0
107200110519     c                   eval         segmento = %Trim(segmento)
107300110519     c                              + ''''
107400110520     c                   end
107500110405      *
107600110316     c                   eval       Conta_righe = 1 + Conta_righe
107700110525      *
107800110525      *  se deve scrivere il file di scarico segmento x ogni linea
107900110525     c                   if        compatta_80  <>'S'
108000110405     c                   exsr      VGD_WRITE
108100110525     c                   else
108200110525      *
108300110525      *  se invece deve scrivere a 80 colonne
108400110525     c                   exsr      WRITE_80COL
108500110525      *
108600110525     c                   end
108700110316      *
108800110316     c                   endsr
108900110405      **?__________________________________________________________________ */
109000110405      *    Apertura del TIVGD
109100110316      **?__________________________________________________________________ */
109200110405     C     VGD_OPEN      Begsr
109300110405      *
109400110617      *  recupera il progressivo
109500110617     c                   exsr      Piglia_progr
109600110617      *
109700110405      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
109800110405     C                   clear                   trul47ds
109900110405     C                   eval      d47opz  = 'I'
110000110617     C                   eval      d47tip  = TipoF
110100110405     C                   eval      d47lck  = 'N'
110200110405     C                   eval      d47chkj = 'N'
110300130419     C                   eval      d47pgm  = 'TRTCT99R'
110400110405     C                   call      'TRUL47R'
110500110405     C                   parm                    trul47ds
110600110405      *
110700110405     c                   endsr
110800110617      **?__________________________________________________________________ */
110900130502      *   Prende il Progressivo
111000130502      **?__________________________________________________________________ */
111100130502     C     Piglia_progr  Begsr
111200130502      *
111300130521     C                   MOVEL     CLIENTE_trs   virKSC
111400130502     C                   MOVEL     TipoF         virTIP
111500130502     c     kvir02        chain     tivir02l
111600130502     c                   move      'OF'          Var_ISV
111700130502     c                   if        %Found(tivir02l)
111800130502     c                   move      VirFI1        Var_ISV
111900130502     C                   End
112000130502
112100130502      *       prende numeratore strategi
112200130502     C                   exsr      calprog
112300130502      *
112400130502      *  Se non ha reperito correttamente il prograssivo
112500130502      *  per la scrittura del TIVGD, potrebbe essere stato richiamato male
112600130502     C                   if        sav_PROGR   = *blank
112700130502???? C                   EVAL      Indirizzi = CED_Bart
112800130502???? C                   EVAL      Oggetto ='Problemi ORM DOWNLOAD EDI -
112900130502     c                             IFTMBP'
113000130502     C                   EVAL      Messaggio = 'ATTENZIONE : -
113100130502     C                             NON agganciato PROGRESSIVO x il TIPO In-
113200130502     C                             vio su DOWNLOAD x il Cliente: ' + Cliente8A
113300130502     c                   exsr      Invio_Mail
113400130502     c                   eval      T99ESITO = '1'
113500130502     c                   end
113600130502      *
113700130502     c                   endsr
113800130502     C*------------------------------------------------------*
113900130502     C*  prepara il file
114000130502     C*------------------------------------------------------*
114100130502     C     calprog       begsr
114200130502     C*
114300130502     C                   open      tis7prgf
114400130502     C                   read(e)   tis7prgf
114500130502     C*
114600130502     C                   if        not %error
114700130502     C                   eval      dwlprg = f_tis7prgf
114800130502     C*
114900130502     C                   move(p)   dwlprg        wrkprg
115000130502     C                   add       1             wrkprg
115100130502     C                   move(p)   wrkprg        dwlprg
115200130502     C                   movel     Var_ISV       dwlprg
115300130502     C*
115400130502     C                   eval       f_tis7prgf = dwlprg
115500130502     c                   eval      SAV_Progr   = dwlprg
115600130502     C*
115700130502     C                   update    tis7prg0
115800130502     C                   endif
115900130502     C*
116000130502     C                   close     tis7prgf
116100130502     C*
116200130502     C                   endsr
116300130502      **?__________________________________________________________________ */
116400110617      *   Scrittura del TIVGD
116500110617      **?__________________________________________________________________ */
116600110617     C     VGD_WRITE     Begsr
116700110617      *
116800110405     c                   clear                   tivgd000
116900110405     c                   eval      vgddta = %TrimR(Segmento)
117000110617     c                   eval      vgdtip = TipoF
117100130521     c                   eval      vgdksu = CLIENTE_trs
117200130502     C                   eval      vgdprg = SAV_Progr
117300110405     c                   eval      vgdtsc = 'WW'
117400130419     c                   eval      vgdpgm = 'TRTCT99R'
117500110405     c                   eval      vgddat = udtymd
117600110405      *
117700110405     C                   WRITE     tivgd000
117800110405      *
117900110405     c                   endsr
118000110405      **?__________________________________________________________________ */
118100110405      *    Chiusura del TIVGD
118200110405      **?__________________________________________________________________ */
118300110405     C     VGD_CLOSE     Begsr
118400110405      *
118500110405     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
118600110405     C                   clear                   trul47ds
118700110405     C                   eval      d47opz  = 'F'
118800110617     C                   eval      d47tip  = TipoF
118900110405     C                   call      'TRUL47R'
119000110405     C                   parm                    trul47ds
119100110405      *
119200110405     c                   endsr
119300110525      **?__________________________________________________________________ */
119400110525      *    Pulisce in QTEMP il file contenitore di 80 colonne
119500110405      **?__________________________________________________________________ */
119600110525     C     CLEAR_STD80F  Begsr
119700110525      *
119800110525      *   crea in QTEMP il EDISTD80F pulito da riempire per compattare
119900110525      *   a 80 colonne.
120000110525     c                   call      'TRTCT00R4C'
120100110525      *
120200110525     c                   endsr
120300110525      **?__________________________________________________________________ */
120400110525      *    Carica in  QTEMP il file contenitore di 80 colonne
120500110525      **?__________________________________________________________________ */
120600110525     C     WRITE_80COL   Begsr
120700110525      *
120800110525      * sposta la stringa del segmento appena composta
120900110525     c                   movel     segmento      segmento1950
121000110525      *
121100130416      *   Aggiunge il segmento al EDISTD80F compattandolo a 80 colonne
121200110525     c                   call      'TRTCT00R4'
121300110525     c                   parm                    segmento1950
121400110525      *
121500110525     c                   endsr
121600110525      **?__________________________________________________________________ */
121700110525      *    Legge il file da QTEMP e lo scarica sul TIVGD x il DOWNLOAD
121800110525      **?__________________________________________________________________ */
121900110525     C     Scarica_su_VGDBegsr
122000110525      *
122100110525     c                   open      EDISTD80F
122200110525     c                   read      EDISTD80F
122300110525      *
122400110525     c                   dow       not %EoF(EDISTD80F)
122500110525      *
122600110525     c                   eval      segmento = RIGA80STD
122700110525     c                   exsr      VGD_WRITE
122800110617      *
122900110525     c                   read      EDISTD80F
123000110525     c                   end
123100110525      *
123200130503     c                   close     EDISTD80F
123300130503      *
123400110525     c                   endsr
123500110617      **?__________________________________________________________________ */
123600110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
123700130415      **?__________________________________________________________________ */
123800110621     C     *pssr         BEGSR
123900110621     C
124000130419     C                   eval      T99ESITO = '1'
124100110621     C                   ENDSR     '*CANCL'
124200110621     C
124300130415      * ?------------------------------------------------------------------ */
124400130415      * CTRL caricamento schiere    PT                               -*
124500130415      * ?------------------------------------------------------------------ */
124600130415     C     Check_PT      begsr
124700130415     C*
124800130415     c* Controllo riempimento schiera
124900130415     c                   clear                   trul0sds
125000130415     c                   eval      i0sski='PT_key'
125100130415     c                   eval      i0sele=%elem(PT_key)
125200130415     c                   eval      i0spie=x
125300130415     c                   eval      i0sfile='EDTAB00F'
125400130415     c                   eval      i0ssif=knsif
125500130419     c                   eval      i0spgm='TRTCT99R'
125600130415     c                   move      kpjbu         SvKpjbu
125700130415     c                   clear                   kpjbu
125800130415     c                   movel     trul0sds      kpjbu
125900130415     c                   call      'TRUL0SR'
126000130415     c                   parm                    kpjba
126100130415     c                   eval      kpjbu = SvKpjbu
126200130415     C*
126300130415     C                   ENDSR
126400130415      * ?------------------------------------------------------------------ */
126500130415      *?    CARICA TABELLE SU SCHIERE
126600130415      * ?------------------------------------------------------------------ */
126700130502     C     CARICA_EDTAB  BEGSR
126800130415      *
126900130415      * Caricamento Tabella Partner esteri
127000130415     C                   Z-ADD     0             X
127100130415     C                   MOVEL     'PT'          TABCOD
127200130415     C     TABCOD        CHAIN     EDTAB01L                           30
127300130415     C     *IN30         DOWEQ     '0'
127400130416      *
127500130415      * carica solo quello che interessa uno .... due ....tre record al max.
127600130415     C                   IF        TABFLG = *BLANKS
127700130415     C                   MOVEL     TABUNI        edidsPT
127800130416      *
127900130418     C                   if          cliente7n = §PTKSC
128000130415     C                   ADD       1             X
128100130415     C                   MOVEL     TABKEY        PT_key(X)
128200130415     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
128300130415     C                   MOVEL     TABUNI        PT_uni(X)
128400130415     C                   eval      PT_ZZ(X)  = §PTMSQ
128500130416      *
128600130416      *   Carica solo il PRIMO TROVATO
128700130416     c                   LEAVE
128800130415     c                   end
128900130416      *
129000130415     C                   END
129100130415     C     TABCOD        READE     EDTAB01L                               30
129200130415     C                   END
129300130415      *
129400130416      *  se non ha trovato nemmeno un record sulla PT lo segnala
129500130416     C                   IF            X = 0
129600130416???? C                   EVAL      Indirizzi = CED_Bart
129700130416???? C                   EVAL      Oggetto ='Problemi EDI DOWNLOAD IFTMBP'
129800130416     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
129900130416     C                             ORM EXPORT - messaggio -
130000130416     C                             UNB non TROVATO in tabella PT controlla-
130100130502     C                             re EDI su DOWNLOAD per il cliente: ' +
130200130502     c                             Cliente8a
130300130416     c                   exsr      Invio_Mail
130400130419     c                   eval      T99ESITO = '1'
130500130416     C                   END
130600130416      *
130700130415      *  se deve inviare la mail di alert x limite quasi raggiunto.
130800130415     c                   exsr      ChecK_PT
130900130415      *
131000130415      * salva quanti Codici UNB ha caricato
131100130415     C                   MOVEL     'PU'          TABCOD
131200130416     C     KediTAB       CHAIN     EDTAB01L
131300130416     C                   if        %Found(EDTAB01L) and
131400130416     C                             TABFLG = *BLANKS
131500130416     C                   MOVEL     TABUNI        edidsPU
131600130418     c                   movel     §pucompat     Compatta_80       1
131700130415     C                   MOVEL     TABKEY        PU_key
131800130415     C                   Z-ADD     1             X
131900130415     C     PU_key        LOOKUP    PT_key(X)                              32
132000130415     C   32              MOVEL     TABUNI        PU_uni(X)
132100130415     C                   END
132200130415      *
132300130415      * Prosegue tab.PT e PU
132400130415     C                   MOVEL     'PV'          TABCOD
132500130416     C     KediTAB       CHAIN     EDTAB01L
132600130416     C                   if        %Found(EDTAB01L) and
132700130416     C                             TABFLG = *BLANKS
132800130416     C                   MOVEL     TABUNI        edidsPV
132900130415     C                   MOVEL     TABKEY        PV_key
133000130415     C                   Z-ADD     1             X
133100130415     C     PV_key        LOOKUP    PT_key(X)                              32
133200130415     C   32              MOVEL     TABUNI        PV_uni(X)
133300130416      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
133400130416      *  ricevuti:
133500130416     c                   movel     §PVema        mail_ind         44
133600130416      * ?imposta gli indirizzi mail se interni a BRT o esterni
133700130416     c                   if        mail_ind <> *blank
133800130416     c                   exsr      imp_ADDR_mail
133900130416     c                   end
134000130416      *
134100130416     C                   END
134200130415      *
134300130415      * Prosegue tab.PT e PU e PV
134400130415     C                   MOVEL     'PZ'          TABCOD
134500130416     C     KediTAB       CHAIN     EDTAB01L
134600130416     C                   if        %Found(EDTAB01L) and
134700130416     C                             TABFLG = *BLANKS
134800130416     C                   MOVEL     TABUNI        edidsPZ
134900130415     C                   MOVEL     TABKEY        PZ_key
135000130415     C                   Z-ADD     1             X
135100130415     C     PZ_key        LOOKUP    PT_key(X)                              32
135200130415     C   32              MOVEL     TABUNI        PZ_uni(X)
135300130521      **
135400130521      * se richiesta la trasmissione ad un codice UNIFICANTE PARTICOLARE
135500130521      **   deve sovrapporsi al codice cliente Generico legato all'UNB
135600130521     c                   if                  §puXXX = 'S' and
135700130521     c                                §PZFTPCOD <> *zeros and
135800130521     c                                §PZFTPCOD <> *blank
135900130521     c                   move      *all'0'       CLIENTE_trs
136000130521     c                   move      §PZFTPCOD     CLIENTE_trs
136100130521     c                   end
136200130521      *
136300130415     C                   END
136400130521      *
136500130521      * Se invece NON aveva questo tipo di invio con ALTRO CODICE imposta
136600130521      *  il codice cliente della TABELLA PT generale come codice trasmissione
136700130521     c                   if        CLIENTE_trs = *blank
136800130521     C                   MOVEL     CLIENTE8a     CLIENTE_trs
136900130521     c                   end
137000130415      *
137100130419      * Prosegue tab.PT e PU e PV e PO
137200130419     C                   MOVEL     'PO'          TABCOD
137300130419     C     KediTAB       CHAIN     EDTAB01L
137400130419     C                   if        %Found(EDTAB01L) and
137500130419     C                             TABFLG = *BLANKS
137600130419     C                   MOVEL     TABUNI        edidsPO
137700130419     C                   MOVEL     TABKEY        PO_key
137800130419     C                   Z-ADD     1             X
137900130419     C     PO_key        LOOKUP    PT_key(X)                              32
138000130419     C   32              MOVEL     TABUNI        PO_uni(X)
138100130419     C                   END
138200130419      *
138300130415     C* Caricamento Tabella codici clienti - tariffe
138400130415     C                   Z-ADD     0             X
138500130415     C                   MOVEL     'CL'          TABCOD
138600130415     C     TABCOD        CHAIN     EDTAB01L                           30
138700130415     C     *IN30         DOWEQ     '0'
138800130415     C                   if        TABFLG = *BLANKS
138900130415     C                   ADD       1             X
139000130415     C                   MOVEL     TABKEY        Cod_Tb_CL(X)
139100130415     C                   MOVEL     TABUNI        Des_Tb_CL(X)
139200130415     C                   END
139300130415     C     TABCOD        READE     EDTAB01L                               30
139400130415     C                   END
139500130415      *
139600130415     C* Caricamento Serivizi speciali
139700130415     C                   Z-ADD     0             X
139800130415     C                   MOVEL     'SS'          TABCOD
139900130415     C     TABCOD        CHAIN     EDTAB01L                           30
140000130415     C     *IN30         DOWEQ     '0'
140100130415     C                   if        TABFLG = *BLANKS
140200130415     C                   ADD       1             X
140300130415     C                   eval       Key_ServSpec(X) = TABKEY
140400130415     C                   eval       SpecialServ(X)  = TABKEY
140500130415     C                   eval       UNI_ServSpec(X) = TABUNI
140600130415     C                   END
140700130415     C     TABCOD        READE     EDTAB01L                               30
140800130415     C                   END
140900130416      *
141000130416     C* Caricamento Tabella 15
141100130416      * Nazione di consegna x P.O. di Consegna ed Emittente
141200130416     C                   z-add     0             Nz                3 0
141300130416     c                   clear                   Naz03
141400130416     c                   clear                   Iso03
141500130416     c                   clear                   Cpf03
141600130418     C                   z-add     1             tblKUT
141700130418     C                   MOVEL(p)  '15'          tblCOD
141800130418     C     KTABel        setll     TABEL00F
141900130418     C     KTABel        reade     TABEL00F
142000130416DO  1C                   Dow       not %EoF(TABEL00F)
142100130416DO  1C                   if         tblflg = *blank
142200130416     C                   MOVEL     TBLuni        DS15
142300130416     C                   add       1             Nz
142400130416     C                   MOVEL     tblKEY        Naz03(Nz)
142500130416     C                   MOVEL     §15COD        Iso03(Nz)
142600130416     C                   MOVEL     §15ECF        CPF03(Nz)
142700130416     C                   MOVE      §15PCF        CPF03(Nz)
142800130416E   1C                   End
142900130418     C     KTABel        reade     TABEL00F
143000130416E   1C                   End
143100130416      *
143200130416     C* Caricamento Tabella Priorità trasporto
143300130416     C                   Z-ADD     0             X                 4 0
143400130416     C                   MOVEL     'TS'          TABCOD
143500130416     C     TABCOD        CHAIN     EDTAB01L                           30
143600130416     C     *IN30         DOWEQ     '0'
143700130416DO  1C                   if         tblflg = *blank
143800130416     C                   ADD       1             X
143900130416     C                   eval      TipoServizio(X) = TABUNI
144000130416     C                   eval      Key_TipServ(X)  = TABKEY
144100130416     C                   END
144200130416     C     TABCOD        READE     EDTAB01L                               30
144300130416     C                   END
144400130415      *
144500130415     C                   ENDSR
144600130416     c*==================================================================*
144700130416      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
144800130416     c*==================================================================*
144900130416     c     Imp_ADDR_mail begsr
145000130416      *
145100130416     c                   clear                   Indirizzi
145200130416      *
145300130416      *  Lunghezza indirizzi presenti
145400130416     c                   eval      Lunghezza_Indirizzo =
145500130416     c                                       %Len(%TrimR(Mail_Ind))
145600130416     c                   z-add     1             al_char           3 0
145700130416     c                   z-add     1             dal_char          3 0
145800130416     c                   z-add     1             tot_char          3 0
145900130416      *
146000130416     c     successivo    tag
146100130416      *
146200130416      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
146300130416      *   e aggiunge il dominio Brt + il (;) separatore
146400130416     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
146500130416     c                   eval      tot_char = al_char - dal_char
146600130416     c                   z-add     dal_char      dalCarattere
146700130416     c                   z-add     tot_char      xTOTcaratteri
146800130416      *
146900130416     c                   clear                   Indir_BRT        40
147000130416     c                   clear                   Indir_Parz       20
147100130416     c                   eval      Indir_Parz =
147200130416     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
147300130416      *
147400130416     c                   eval      Indir_BRT    = %Trim(Indir_Parz) +
147500130416     c                             %Trim(bart_it)
147600130416     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_BRT
147700130416      *
147800130416     c                   if        al_char = Lunghezza_Indirizzo
147900130416     c                   goto      fine_indmail
148000130416     c                   else
148100130416     c                   eval      dal_char = ( al_char + 1 )
148200130416     c                   goto      successivo
148300130416     c                   end
148400130416      *
148500130416     C     fine_indmail  TAG
148600130416      *
148700130416     C                   ENDSR
148800130416     c*==================================================================*
148900130416      * Manda un Msg x E-mail
149000130416     c*==================================================================*
149100130416     c     Invio_Mail    begsr
149200130416      *
149300130416     C*   Alert_mail : invio Mail a CED@Brt.it                  *
149400130416     C* Inizializzo variabili
149500130416     C                   movel     *blanks       wrkEml          253
149600130416     C                   movel     *blanks       wrkMsg         5000
149700130416     C                   movel     *blanks       wrkOgg           44
149800130416      *
149900130416     C* Valorizzo i campi della e-m@ail - indirizzo
150000130416     C*******            eval      wrkEml = 'Andrea.Bertocchi@Brt.it'
150100130416     C                   eval      wrkEml = Indirizzi
150200130416     C                   eval      wrkOgg = Oggetto
150300130416     C                   eval      wrkMsg = Messaggio
150400130416      ** *****
150500130416     C                   call(e)   'TRTCT00R2'
150600130416     C                   parm                    wrkEml
150700130416     C                   parm                    wrkOgg
150800130416     C                   parm                    wrkMsg
150900130416     C*
151000130416     C                   ENDSR
151100130415      * ?------------------------------------------------------------------ */
151200130502     C* Solo la prima volta  aprendo in RT
151300130502      * ?------------------------------------------------------------------ */
151400130502     C     *inzsr        begSR
151500130502     C*
151600130502     C                   ENDSR
151700130502      * ?------------------------------------------------------------------ */
