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
022600110518     D UNB_sgm         C                   CONST('UNB+UNOA:2+ITBAR005:ZZ+')
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
037600130415     c                   eval        UNB0001   = 'UNOA'
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
061200130418     c                   clear                   eds00FTX
061300130418     c                   eval       FTX4451 = 'AAA'
061400130418     c                   eval       FTX44401= ORMNAM
061500130418      **  [scrive TIVGD]
061600130415     c                   exsr      segm_FTX
061700130419     c                   end
061800130418      *  _____________GID
061900130418     c                   clear                   eds00GID
062000130418     c                   eval       GID1496   = 99999
062100130828      *
062200130828      * come unità sono i colli + i bancali
062300130828     c                   if        ormNCL + ormBNC >  0
062400130419     c                   eval       GID706520 = 'PK'
062500130828     c                   eval       GID722420 = ORMNCL + ORMBNC
062600130828     c                   end
062700130828      * Bancali
062800130828     c                   if        ormBNC >  0
062900130828     c                   eval       GID706530 = 'EUR'
063000130828     c                   eval       GID722430 = ORMBNC
063100130419     c                   end
063200130418      **  [scrive TIVGD]
063300130418     c                   exsr      segm_GID
063400130418      *  _____________FTX
063500130419     c                   if        ORMNO1 <> *blank
063600130418     c                   clear                   eds00FTX
063700130418     c                   eval       FTX4451 = 'ACB'
063800130418     c                   eval       FTX44401= ORMNO1
063900130418      **  [scrive TIVGD]
064000130415     c                   exsr      segm_FTX
064100130419     c                   end
064200130418      *  _____________FTX
064300130419     c                   if        ORMNO2 <> *blank
064400130418     c                   clear                   eds00FTX
064500130418     c                   eval       FTX4451 = 'ACB'
064600130418     c                   eval       FTX44401= ORMNO2
064700130418      **  [scrive TIVGD]
064800130418     c                   exsr      segm_FTX
064900130419     c                   end
065000130502      *  _____________FTX
065100130502     c                   if        ORMrfa <> *blank
065200130502     c                   clear                   eds00FTX
065300130502     c                   eval       FTX4451 = 'ACB'
065400130502     c                   eval       FTX44401= ORMrfa
065500130502      **  [scrive TIVGD]
065600130502     c                   exsr      segm_FTX
065700130502     c                   end
065800130418      *  _____________MEA
065900130418     c                   clear                   eds00MEA
066000130419     c                   z-add     ORMPKG        MEA6314
066100130418      **  [scrive TIVGD]
066200130415     c                   exsr      segm_MEA
066300130415
066400130418      **  [scrive TIVGD]
066500130415     c                   exsr      segm_UNT
066600130415
066700130415      * conferma la scrittura del TIVGD
066800130415     c                   commit
066900130415      *
067000130415     C                   Endsr
067100110316      **?------------------------------------------------------------------ */
067200110316      **    Scrittura  del segmento UNA                Edi Standard
067300110316      **?------------------------------------------------------------------ */
067400110316     C     Segm_UNA      Begsr
067500110525      *
067600110525      *  Se deve compattare pulisce il file di lavoro in QTEMP EDISTD80F
067700110525     c                   if        compatta_80  ='S'
067800110525     C                   EXSR      CLEAR_STD80F
067900110525     c                   end
068000110525      *
068100110318     c                   eval         segmento = UNA_sgm
068200110518     C                   EXSR      TIVGD
068300110316      *
068400110316     C                   Endsr
068500110316      **?------------------------------------------------------------------ */
068600110316      **    Scrittura  del segmento UNB                Edi Standard
068700110316      **?------------------------------------------------------------------ */
068800110316     C     Segm_UNB      Begsr
068900110316      *
069000110318     c                   eval         segmento = UNB_sgm
069100130415     c                              + %Trim(UNB0010)
069200110519      *
069300110322      *  Aggiunge il qualificatore se c'è
069400130415     c                   if           UNB000730 <> *blank
069500110322     c                   eval         segmento = %Trim(segmento)
069600110316     c                              + ':'
069700130415     c                              + %Trim(UNB000730)
069800110322     c                   end
069900110519      *
070000110322     c                   eval         segmento = %Trim(segmento)
070100110316     c                              + '+'
070200130415     c                              + %Trim(%editc(UNB0017:'X'))
070300110316     c                              + ':'
070400130415     c                              + %Editc(UNB0019:'X')
070500110316     c                              + '+'
070600130415     c                              + %Trim(UNB0020)
070700110316
070800110518     C                   EXSR      TIVGD
070900110316      *
071000130503     c                   eval      Conta_UNH_UNT = 0
071100130503      *
071200110316     C                   Endsr
071300110316      **?------------------------------------------------------------------ */
071400110316      **    Scrittura  del segmento UNH                Edi Standard
071500110316      **?------------------------------------------------------------------ */
071600110316     C     Segm_UNH      Begsr
071700110316      *
071800130503     c                   eval         da_UNH_a_UNT = 1
071900130503     c                   eval      Conta_UNH_UNT = Conta_UNH_UNT +1
072000130503     c                   eval      UNH0062 = %editc(Conta_UNH_UNT:'X')
072100130503      *
072200110318     c                   eval         segmento = UNH_sgm
072300130503     c                              + %Trim(UNH0062)
072400130503     c                              + '+'
072500130503     c                              + %Trim(UNH0065)
072600130503     c                              + ':'
072700130503     c                              + %Trim(UNH0052)
072800130503     c                              + ':'
072900130503     c                              + %Trim(UNH0054)
073000130503     c                              + ':'
073100130503     c                              + %Trim(UNH0051)
073200110316
073300110518     C                   EXSR      TIVGD
073400110316      *
073500110316     C                   Endsr
073600110316      **?------------------------------------------------------------------ */
073700110316      **    Scrittura  del segmento BGM                Edi Standard
073800110316      **?------------------------------------------------------------------ */
073900110316     C     Segm_BGM      Begsr
074000110316      *
074100110318     c                   eval         segmento = BGM_sgm
074200130415     c                              + %Trim(BGM1001)
074300110519     c                              + '+'
074400130415     c                              + %Trim(BGM1004)
074500110316     c                              + '+'
074600130415     c                              + %Trim(BGM1225)
074700110316
074800110518     C                   EXSR      TIVGD
074900110316      *
075000110316     C                   Endsr
075100130418      **?------------------------------------------------------------------ */
075200130418      **    Scrittura  del segmento DTM                Edi Standard
075300130418      **?------------------------------------------------------------------ */
075400130418     C     Segm_DTM      Begsr
075500130418      *
075600130418     c                   if         DTM2005 <> *blank
075700130418     c                   eval         segmento = DTM_sgm
075800130418     c                              + %Trim(DTM2005)
075900130418     c                              + ':'
076000130418     c                              + %Trim(DTM2380)
076100130418     c                              + ':'
076200130418     c                              + %Trim(DTM2379)
076300130418      *
076400130418     C                   EXSR      TIVGD
076500130418     c                   end
076600130418      *
076700130418     C                   Endsr
076800130418      **?------------------------------------------------------------------ */
076900130418      **    Scrittura  del segmento TDT                Edi Standard
077000130418      **?------------------------------------------------------------------ */
077100130418     C     Segm_TDT      Begsr
077200130418      ***
077300130418     c                   eval         segmento = TDT_sgm
077400130418      ***
077500130418     C                   EXSR      TIVGD
077600130418      *
077700130418     C                   Endsr
077800130418      **?------------------------------------------------------------------ */
077900130418      **    Scrittura  del segmento TSR                Edi Standard
078000130418      **?------------------------------------------------------------------ */
078100130418     C     Segm_TSR      Begsr
078200130418      *
078300130418     c                   eval         segmento = TSR_sgm
078400130419      *   se c'è il Fermo Deposito
078500130419     c                   if        ORMFFD = 'S'
078600130419     c                   eval         segmento = %trim(segmento)
078700130419     c                                + '+GWC'
078800130419     c                   end
078900130418      *
079000130418     C                   EXSR      TIVGD
079100130418      *
079200130418     C                   Endsr
079300130418      **?------------------------------------------------------------------ */
079400130418      **    Scrittura  del segmento LOC                Edi Standard
079500130418      **?------------------------------------------------------------------ */
079600130418     C     Segm_LOC      Begsr
079700130418      *
079800130418     c                   eval         segmento = LOC_sgm
079900130418      *
080000130418     C                   EXSR      TIVGD
080100130418      *
080200130418     C                   Endsr
080300130418      **?------------------------------------------------------------------ */
080400130418      **    Scrittura  del segmento NAD                Edi Standard
080500130418      **?------------------------------------------------------------------ */
080600130418     C     Segm_NAD      Begsr
080700130418      *
080800130418     c                   eval         segmento = NAD_sgm
080900130418      *
081000130418     c                   eval      campo_alfa = NAD3039
081100130418     c                   eval      lunghezza_campo=%len(NAD3039)
081200130418     c                   exsr      Char_Speciali
081300130418     c                   eval      NAD3039 = campo_alfa
081400130418      *
081500130418     c                   eval      campo_alfa = NAD30361
081600130418     c                   eval      lunghezza_campo=%len(NAD30361)
081700130418     c                   exsr      Char_Speciali
081800130418     c                   eval      NAD30361 = campo_alfa
081900130418      *
082000130418     c                   eval      campo_alfa = NAD30421
082100130418     c                   eval      lunghezza_campo=%len(NAD30421)
082200130418     c                   exsr      Char_Speciali
082300130418     c                   eval      NAD30421 = campo_alfa
082400130418      *
082500130418     c                   eval      campo_alfa = NAD3164
082600130418     c                   eval      lunghezza_campo=%len(NAD3164)
082700130418     c                   exsr      Char_Speciali
082800130418     c                   eval      NAD3164 = campo_alfa
082900130418      *
083000130418     c                   eval      campo_alfa = NAD3251
083100130418     c                   eval      lunghezza_campo=%len(NAD3251)
083200130418     c                   exsr      Char_Speciali
083300130418     c                   eval      NAD3251 = campo_alfa
083400130418      *
083500130418     c                   eval         segmento = %Trim(segmento)
083600130418     c                              + %Trim(NAD3035)
083700130418     c                              + '+'
083800130521     c*********                     + %Trim(NAD3039)
083900130418     c                              + '+'
084000130418     c                              + '+'
084100130418     c                              + %Trim(NAD30361)
084200130418     c                              + '+'
084300130418     c                              + %Trim(NAD30421)
084400130418     c                              + '+'
084500130418     c                              + %Trim(NAD3164)
084600130418     c                              + '+'
084700130418     c                              + '+'
084800130418     c                              + %Trim(NAD3251)
084900130418     c                              + '+'
085000130418     c                              + %Trim(NAD3207)
085100130418      *
085200130418     C                   EXSR      TIVGD
085300130418      *
085400130418     C                   Endsr
085500130418      **?------------------------------------------------------------------ */
085600130418      **    Scrittura  del segmento RFF                Edi Standard
085700130418      **?------------------------------------------------------------------ */
085800130418     C     Segm_RFF      Begsr
085900130418      *
086000130418     c                   if        RFF1153 <> *blank
086100130418     c                   eval         segmento = RFF_sgm
086200130418     c                              + %Trim(RFF1153)
086300130418     c                              + ':'
086400130418     c                              + %Trim(RFF1154)
086500130418      *
086600130418     C                   EXSR      TIVGD
086700130418     c                   end
086800130418      *
086900130418     C                   Endsr
087000130418      **?------------------------------------------------------------------ */
087100130418      **    Scrittura  del segmento CTA                Edi Standard
087200130418      **?------------------------------------------------------------------ */
087300130418     C     Segm_CTA      Begsr
087400130418      *
087500130418     c                   eval         segmento = CTA_sgm
087600130418      *
087700130418     c                   eval      campo_alfa = CTA3412
087800130418     c                   eval      lunghezza_campo=%len(CTA3412)
087900130418     c                   exsr      Char_Speciali
088000130418     c                   eval      CTA3412 = campo_alfa
088100130418      *
088200130418     c                   eval         segmento = %Trim(segmento)
088300130418     c                              + %Trim(CTA3139)
088400130418     c                              + '+:'
088500130418     c                              + %Trim(CTA3412)
088600130418
088700130418     C                   EXSR      TIVGD
088800130418      *
088900130418     C                   Endsr
089000130418      **?------------------------------------------------------------------ */
089100130418      **    Scrittura  del segmento COM                Edi Standard
089200130418      **?------------------------------------------------------------------ */
089300130418     C     Segm_COM      Begsr
089400130418      *
089500130418     c                   eval         segmento = COM_sgm
089600130418      *
089700130418     c                   eval      campo_alfa = COM3148
089800130418     c                   eval      lunghezza_campo=%len(COM3148)
089900130418     c                   exsr      Char_Speciali
090000130418     c                   eval      COM3148 = campo_alfa
090100130418      *
090200130418     c                   eval         segmento = %Trim(segmento)
090300130418     c                              + %Trim(COM3148)
090400130418      *
090500130418     c                   if        COM3155 <> ' '
090600130418     c                   eval         segmento = %Trim(segmento)
090700130418     c                              + ':'
090800130418     c                              + %Trim(COM3155)
090900130418     c                   end
091000130418
091100130418     C                   EXSR      TIVGD
091200130418      *
091300130418     C                   Endsr
091400110519      **?------------------------------------------------------------------ */
091500110519      **    Scrittura  del segmento FTX                Edi Standard
091600110519      **?------------------------------------------------------------------ */
091700110519     C     Segm_FTX      Begsr
091800110519      *
091900130418     c                   if         FTX4451 <> *blank
092000110524      *
092100130418     c                   eval      campo_alfa = FTX44401
092200130418     c                   eval      lunghezza_campo=%len(FTX44401)
092300110524     c                   exsr      Char_Speciali
092400130418     c                   eval      FTX44401= campo_alfa
092500110524      *
092600110519     c                   eval         segmento = FTX_sgm
092700130418     c                              + %Trim(FTX4451)
092800110519     c                              + '+++'
092900130418     c                              + %Trim(FTX44401)
093000110519      *
093100110519     C                   EXSR      TIVGD
093200110519     c                   end
093300110519      *
093400110519     C                   Endsr
093500130418      **?------------------------------------------------------------------ */
093600130418      **    Scrittura  del segmento GID                Edi Standard
093700130418      **?------------------------------------------------------------------ */
093800130418     C     Segm_GID      Begsr
093900130418      *
094000130418     c                   eval         segmento = GID_sgm
094100130418     c                              + %Trim(%EDITC(GID1496:'Z'))
094200130418     c                              + '+'
094300130418     c                              + %Trim(%EDITC(GID722420:'Z'))
094400130418     c                              + ':'
094500130418     c                              + %Trim(GID706520)
094600130828      * se ci sono i BANCALI
094700130828     c                   if         GID706530 <> *blank
094800130828     c                   eval         segmento = %trim(segmento)
094900130828     c                              + '+'
095000130828     c                              + %Trim(%EDITC(GID722430:'Z'))
095100130828     c                              + ':'
095200130828     c                              + %Trim(GID706530)
095300130828     C                   End
095400130418
095500130418     C                   EXSR      TIVGD
095600130418      *
095700130418     C                   Endsr
095800110316      **?------------------------------------------------------------------ */
095900110519      **    Scrittura  del segmento MEA                Edi Standard
096000110316      **?------------------------------------------------------------------ */
096100110519     C     Segm_MEA      Begsr
096200110520      *
096300130419     c                   clear                   campo18          18
096400130419     c                   eval       campo18 = %editc(MEA6314:'Z')
096500110316      *
096600110519     c                   eval         segmento = MEA_sgm
096700130419     c                              + %Trim(CAMPO18)
096800110316      **
096900110316     C                   EXSR      TIVGD
097000110316      *
097100110316     C                   Endsr
097200110316      **?------------------------------------------------------------------ */
097300110316      **    Scrittura  del segmento UNT                Edi Standard
097400110316      **?------------------------------------------------------------------ */
097500110316     C     Segm_UNT      Begsr
097600110316      *
097700110316     c                   eval         segmento = UNT_sgm
097800130503     c                              + %Trim(%EDITC(da_UNH_a_UNT:'Z'))
097900130503     c                              + '+' + UNH0062
098000110316
098100110316     C                   EXSR      TIVGD
098200110316      *
098300110316     C                   Endsr
098400110316      **?------------------------------------------------------------------ */
098500110316      **    Scrittura  del segmento UNZ                Edi Standard
098600110316      **?------------------------------------------------------------------ */
098700110316     C     Segm_UNZ      Begsr
098800110316      *
098900110316     c                   eval         segmento = UNZ_sgm
099000130503     c                              + %Trim(UNH0062)
099100130503     c                              + '+'
099200130503     c                              + %Trim(UNB0020)
099300110316
099400110316     C                   EXSR      TIVGD
099500110525      *
099600110525      ** a questo punto scarica quello che ha compattato sul TIVGD x il DOWNLOAD
099700110525     c                   if        compatta_80  = 'S'
099800110525     c                   exsr      Scarica_su_VGD
099900110525     c                   end
100000110316      *
100100110316     C                   Endsr
100200110316      **?------------------------------------------------------------------ */
100300110405      **    FINTO ROLLBACK
100400110316      **?------------------------------------------------------------------ */
100500110405     C     ROLL_BCK      Begsr
100600110316      *
100700110405      *  E' meglio non gestire un ROLLBACK FISICO sul TIVGD poichè il file
100800110405      *  è troppo delicato nella sua funzione di DOWNLOAD.
100900110405      *   Se NON SI DEVE scrivere nulla poichè non si è rilevato nessuno stato
101000110405      *   da inviare al cliente, al momento si esce dal pgm senza aver fatto nulla
101100110405      *   Altrimenti questa è la routine per gestire qualche azione da compiere.
101200110405      *
101300110405      *
101400110405     C                   Endsr
101500110520      **?------------------------------------------------------------------ */
101600110520      **    Caratteri particolari
101700110520      **?------------------------------------------------------------------ */
101800110520     C     Char_Speciali Begsr
101900110520      *
102000110520      *  Controlla su campi alfanumerici la presenza di caratteri Speciali
102100110520      *   dichiarati nei segmenti    (+:'?)
102200110520      *   [più, due punti, apostrofo, punto interrogativo]
102300110520      *  Sono i caratteri che servono anche alla struttura EDI e quindi
102400110520      *   considerati caratteri Funzionali che, qualora debbano essere
102500110520      *   utilizzati come semplici caratteri di testo, dovranno essere
102600110520      *   preceduti da un (?).
102700110520      *
102800110520     c                   movel(p)  campo_alfa    campo_test
102900110520     c                   clear                   campo_ritorno
103000110520      *
103100110520     C                   call      'TRTCT00R3'
103200110520     c                   parm                    campo_test      512
103300110520     c                   parm                    campo_ritorno   512
103400130419     c                   parm                    ESITO             1
103500130419     c                   EVAL      T99ESITO = ESITO
103600110520      *
103700110520     c                   movel(p)  campo_ritorno campo_alfa      512
103800111018      ******
103900111018      *  occorre fare attenzione se l'ultimo carattere è un (?) che precede
104000111018      *   uno dei carratteri speciali, questo può procurare problemi a chi riceve
104100111018      *    quindi si deve sostituire il (?) con un blank e togliere tutto ciò che
104200111018      *   sta sulla destra da lì in poi.
104300111018     c                   if        %subst(campo_alfa:lunghezza_campo-1:1) <>
104400111018     c                                     Punto_interrogativo   and
104500111018     c                             %subst(campo_alfa:lunghezza_campo:1)
104600111018     c                                 =   Punto_interrogativo
104700111018      *   Sostituisce il (?) nell'ultimo byte del campo se non aveva nel byte
104800111018      *    precedente un (?) e gli mette blank al posto del (?)
104900111018     c                   eval      %subst(campo_alfa:lunghezza_campo:1)
105000111018     c                                 =   un_blank
105100111018      * si è fatto ciò poichè altrimenti quando si va a comporre il segmento EDI
105200111018      *  il carattere funzionale (+ o : o ') potrebbero venire annullati dalla
105300111018      *  presenza di un (?) davanti creando problemi di traduzione.
105400111018     c                   end
105500110520      *
105600110520     C                   Endsr
105700110405      **?------------------------------------------------------------------ */
105800110405      **    Scrittura  del segmento                    Edi Standard
105900110405      **?------------------------------------------------------------------ */
106000110405     C     tivgd         Begsr
106100110519      *
106200130503      *  conta righe in un dettaglio
106300130503     c                   eval         da_UNH_a_UNT = 1 + da_UNH_a_UNT
106400130503      *
106500110519      *   chiude il segmento con l'apostrofo
106600110520     c                   if         Conta_righe > 0
106700110519     c                   eval         segmento = %Trim(segmento)
106800110519     c                              + ''''
106900110520     c                   end
107000110405      *
107100110316     c                   eval       Conta_righe = 1 + Conta_righe
107200110525      *
107300110525      *  se deve scrivere il file di scarico segmento x ogni linea
107400110525     c                   if        compatta_80  <>'S'
107500110405     c                   exsr      VGD_WRITE
107600110525     c                   else
107700110525      *
107800110525      *  se invece deve scrivere a 80 colonne
107900110525     c                   exsr      WRITE_80COL
108000110525      *
108100110525     c                   end
108200110316      *
108300110316     c                   endsr
108400110405      **?__________________________________________________________________ */
108500110405      *    Apertura del TIVGD
108600110316      **?__________________________________________________________________ */
108700110405     C     VGD_OPEN      Begsr
108800110405      *
108900110617      *  recupera il progressivo
109000110617     c                   exsr      Piglia_progr
109100110617      *
109200110405      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
109300110405     C                   clear                   trul47ds
109400110405     C                   eval      d47opz  = 'I'
109500110617     C                   eval      d47tip  = TipoF
109600110405     C                   eval      d47lck  = 'N'
109700110405     C                   eval      d47chkj = 'N'
109800130419     C                   eval      d47pgm  = 'TRTCT99R'
109900110405     C                   call      'TRUL47R'
110000110405     C                   parm                    trul47ds
110100110405      *
110200110405     c                   endsr
110300110617      **?__________________________________________________________________ */
110400130502      *   Prende il Progressivo
110500130502      **?__________________________________________________________________ */
110600130502     C     Piglia_progr  Begsr
110700130502      *
110800130521     C                   MOVEL     CLIENTE_trs   virKSC
110900130502     C                   MOVEL     TipoF         virTIP
111000130502     c     kvir02        chain     tivir02l
111100130502     c                   move      'OF'          Var_ISV
111200130502     c                   if        %Found(tivir02l)
111300130502     c                   move      VirFI1        Var_ISV
111400130502     C                   End
111500130502
111600130502      *       prende numeratore strategi
111700130502     C                   exsr      calprog
111800130502      *
111900130502      *  Se non ha reperito correttamente il prograssivo
112000130502      *  per la scrittura del TIVGD, potrebbe essere stato richiamato male
112100130502     C                   if        sav_PROGR   = *blank
112200130502???? C                   EVAL      Indirizzi = CED_Bart
112300130502???? C                   EVAL      Oggetto ='Problemi ORM DOWNLOAD EDI -
112400130502     c                             IFTMBP'
112500130502     C                   EVAL      Messaggio = 'ATTENZIONE : -
112600130502     C                             NON agganciato PROGRESSIVO x il TIPO In-
112700130502     C                             vio su DOWNLOAD x il Cliente: ' + Cliente8A
112800130502     c                   exsr      Invio_Mail
112900130502     c                   eval      T99ESITO = '1'
113000130502     c                   end
113100130502      *
113200130502     c                   endsr
113300130502     C*------------------------------------------------------*
113400130502     C*  prepara il file
113500130502     C*------------------------------------------------------*
113600130502     C     calprog       begsr
113700130502     C*
113800130502     C                   open      tis7prgf
113900130502     C                   read(e)   tis7prgf
114000130502     C*
114100130502     C                   if        not %error
114200130502     C                   eval      dwlprg = f_tis7prgf
114300130502     C*
114400130502     C                   move(p)   dwlprg        wrkprg
114500130502     C                   add       1             wrkprg
114600130502     C                   move(p)   wrkprg        dwlprg
114700130502     C                   movel     Var_ISV       dwlprg
114800130502     C*
114900130502     C                   eval       f_tis7prgf = dwlprg
115000130502     c                   eval      SAV_Progr   = dwlprg
115100130502     C*
115200130502     C                   update    tis7prg0
115300130502     C                   endif
115400130502     C*
115500130502     C                   close     tis7prgf
115600130502     C*
115700130502     C                   endsr
115800130502      **?__________________________________________________________________ */
115900110617      *   Scrittura del TIVGD
116000110617      **?__________________________________________________________________ */
116100110617     C     VGD_WRITE     Begsr
116200110617      *
116300110405     c                   clear                   tivgd000
116400110405     c                   eval      vgddta = %TrimR(Segmento)
116500110617     c                   eval      vgdtip = TipoF
116600130521     c                   eval      vgdksu = CLIENTE_trs
116700130502     C                   eval      vgdprg = SAV_Progr
116800110405     c                   eval      vgdtsc = 'WW'
116900130419     c                   eval      vgdpgm = 'TRTCT99R'
117000110405     c                   eval      vgddat = udtymd
117100110405      *
117200110405     C                   WRITE     tivgd000
117300110405      *
117400110405     c                   endsr
117500110405      **?__________________________________________________________________ */
117600110405      *    Chiusura del TIVGD
117700110405      **?__________________________________________________________________ */
117800110405     C     VGD_CLOSE     Begsr
117900110405      *
118000110405     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
118100110405     C                   clear                   trul47ds
118200110405     C                   eval      d47opz  = 'F'
118300110617     C                   eval      d47tip  = TipoF
118400110405     C                   call      'TRUL47R'
118500110405     C                   parm                    trul47ds
118600110405      *
118700110405     c                   endsr
118800110525      **?__________________________________________________________________ */
118900110525      *    Pulisce in QTEMP il file contenitore di 80 colonne
119000110405      **?__________________________________________________________________ */
119100110525     C     CLEAR_STD80F  Begsr
119200110525      *
119300110525      *   crea in QTEMP il EDISTD80F pulito da riempire per compattare
119400110525      *   a 80 colonne.
119500110525     c                   call      'TRTCT00R4C'
119600110525      *
119700110525     c                   endsr
119800110525      **?__________________________________________________________________ */
119900110525      *    Carica in  QTEMP il file contenitore di 80 colonne
120000110525      **?__________________________________________________________________ */
120100110525     C     WRITE_80COL   Begsr
120200110525      *
120300110525      * sposta la stringa del segmento appena composta
120400110525     c                   movel     segmento      segmento1950
120500110525      *
120600130416      *   Aggiunge il segmento al EDISTD80F compattandolo a 80 colonne
120700110525     c                   call      'TRTCT00R4'
120800110525     c                   parm                    segmento1950
120900110525      *
121000110525     c                   endsr
121100110525      **?__________________________________________________________________ */
121200110525      *    Legge il file da QTEMP e lo scarica sul TIVGD x il DOWNLOAD
121300110525      **?__________________________________________________________________ */
121400110525     C     Scarica_su_VGDBegsr
121500110525      *
121600110525     c                   open      EDISTD80F
121700110525     c                   read      EDISTD80F
121800110525      *
121900110525     c                   dow       not %EoF(EDISTD80F)
122000110525      *
122100110525     c                   eval      segmento = RIGA80STD
122200110525     c                   exsr      VGD_WRITE
122300110617      *
122400110525     c                   read      EDISTD80F
122500110525     c                   end
122600110525      *
122700130503     c                   close     EDISTD80F
122800130503      *
122900110525     c                   endsr
123000110617      **?__________________________________________________________________ */
123100110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
123200130415      **?__________________________________________________________________ */
123300110621     C     *pssr         BEGSR
123400110621     C
123500130419     C                   eval      T99ESITO = '1'
123600110621     C                   ENDSR     '*CANCL'
123700110621     C
123800130415      * ?------------------------------------------------------------------ */
123900130415      * CTRL caricamento schiere    PT                               -*
124000130415      * ?------------------------------------------------------------------ */
124100130415     C     Check_PT      begsr
124200130415     C*
124300130415     c* Controllo riempimento schiera
124400130415     c                   clear                   trul0sds
124500130415     c                   eval      i0sski='PT_key'
124600130415     c                   eval      i0sele=%elem(PT_key)
124700130415     c                   eval      i0spie=x
124800130415     c                   eval      i0sfile='EDTAB00F'
124900130415     c                   eval      i0ssif=knsif
125000130419     c                   eval      i0spgm='TRTCT99R'
125100130415     c                   move      kpjbu         SvKpjbu
125200130415     c                   clear                   kpjbu
125300130415     c                   movel     trul0sds      kpjbu
125400130415     c                   call      'TRUL0SR'
125500130415     c                   parm                    kpjba
125600130415     c                   eval      kpjbu = SvKpjbu
125700130415     C*
125800130415     C                   ENDSR
125900130415      * ?------------------------------------------------------------------ */
126000130415      *?    CARICA TABELLE SU SCHIERE
126100130415      * ?------------------------------------------------------------------ */
126200130502     C     CARICA_EDTAB  BEGSR
126300130415      *
126400130415      * Caricamento Tabella Partner esteri
126500130415     C                   Z-ADD     0             X
126600130415     C                   MOVEL     'PT'          TABCOD
126700130415     C     TABCOD        CHAIN     EDTAB01L                           30
126800130415     C     *IN30         DOWEQ     '0'
126900130416      *
127000130415      * carica solo quello che interessa uno .... due ....tre record al max.
127100130415     C                   IF        TABFLG = *BLANKS
127200130415     C                   MOVEL     TABUNI        edidsPT
127300130416      *
127400130418     C                   if          cliente7n = §PTKSC
127500130415     C                   ADD       1             X
127600130415     C                   MOVEL     TABKEY        PT_key(X)
127700130415     C                   eval      PT_Parz(X) = %subst(TABKEY:1:31)
127800130415     C                   MOVEL     TABUNI        PT_uni(X)
127900130415     C                   eval      PT_ZZ(X)  = §PTMSQ
128000130416      *
128100130416      *   Carica solo il PRIMO TROVATO
128200130416     c                   LEAVE
128300130415     c                   end
128400130416      *
128500130415     C                   END
128600130415     C     TABCOD        READE     EDTAB01L                               30
128700130415     C                   END
128800130415      *
128900130416      *  se non ha trovato nemmeno un record sulla PT lo segnala
129000130416     C                   IF            X = 0
129100130416???? C                   EVAL      Indirizzi = CED_Bart
129200130416???? C                   EVAL      Oggetto ='Problemi EDI DOWNLOAD IFTMBP'
129300130416     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
129400130416     C                             ORM EXPORT - messaggio -
129500130416     C                             UNB non TROVATO in tabella PT controlla-
129600130502     C                             re EDI su DOWNLOAD per il cliente: ' +
129700130502     c                             Cliente8a
129800130416     c                   exsr      Invio_Mail
129900130419     c                   eval      T99ESITO = '1'
130000130416     C                   END
130100130416      *
130200130415      *  se deve inviare la mail di alert x limite quasi raggiunto.
130300130415     c                   exsr      ChecK_PT
130400130415      *
130500130415      * salva quanti Codici UNB ha caricato
130600130415     C                   MOVEL     'PU'          TABCOD
130700130416     C     KediTAB       CHAIN     EDTAB01L
130800130416     C                   if        %Found(EDTAB01L) and
130900130416     C                             TABFLG = *BLANKS
131000130416     C                   MOVEL     TABUNI        edidsPU
131100130418     c                   movel     §pucompat     Compatta_80       1
131200130415     C                   MOVEL     TABKEY        PU_key
131300130415     C                   Z-ADD     1             X
131400130415     C     PU_key        LOOKUP    PT_key(X)                              32
131500130415     C   32              MOVEL     TABUNI        PU_uni(X)
131600130415     C                   END
131700130415      *
131800130415      * Prosegue tab.PT e PU
131900130415     C                   MOVEL     'PV'          TABCOD
132000130416     C     KediTAB       CHAIN     EDTAB01L
132100130416     C                   if        %Found(EDTAB01L) and
132200130416     C                             TABFLG = *BLANKS
132300130416     C                   MOVEL     TABUNI        edidsPV
132400130415     C                   MOVEL     TABKEY        PV_key
132500130415     C                   Z-ADD     1             X
132600130415     C     PV_key        LOOKUP    PT_key(X)                              32
132700130415     C   32              MOVEL     TABUNI        PV_uni(X)
132800130416      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
132900130416      *  ricevuti:
133000130416     c                   movel     §PVema        mail_ind         44
133100130416      * ?imposta gli indirizzi mail se interni a BRT o esterni
133200130416     c                   if        mail_ind <> *blank
133300130416     c                   exsr      imp_ADDR_mail
133400130416     c                   end
133500130416      *
133600130416     C                   END
133700130415      *
133800130415      * Prosegue tab.PT e PU e PV
133900130415     C                   MOVEL     'PZ'          TABCOD
134000130416     C     KediTAB       CHAIN     EDTAB01L
134100130416     C                   if        %Found(EDTAB01L) and
134200130416     C                             TABFLG = *BLANKS
134300130416     C                   MOVEL     TABUNI        edidsPZ
134400130415     C                   MOVEL     TABKEY        PZ_key
134500130415     C                   Z-ADD     1             X
134600130415     C     PZ_key        LOOKUP    PT_key(X)                              32
134700130415     C   32              MOVEL     TABUNI        PZ_uni(X)
134800130521      **
134900130521      * se richiesta la trasmissione ad un codice UNIFICANTE PARTICOLARE
135000130521      **   deve sovrapporsi al codice cliente Generico legato all'UNB
135100130521     c                   if                  §puXXX = 'S' and
135200130521     c                                §PZFTPCOD <> *zeros and
135300130521     c                                §PZFTPCOD <> *blank
135400130521     c                   move      *all'0'       CLIENTE_trs
135500130521     c                   move      §PZFTPCOD     CLIENTE_trs
135600130521     c                   end
135700130521      *
135800130415     C                   END
135900130521      *
136000130521      * Se invece NON aveva questo tipo di invio con ALTRO CODICE imposta
136100130521      *  il codice cliente della TABELLA PT generale come codice trasmissione
136200130521     c                   if        CLIENTE_trs = *blank
136300130521     C                   MOVEL     CLIENTE8a     CLIENTE_trs
136400130521     c                   end
136500130415      *
136600130419      * Prosegue tab.PT e PU e PV e PO
136700130419     C                   MOVEL     'PO'          TABCOD
136800130419     C     KediTAB       CHAIN     EDTAB01L
136900130419     C                   if        %Found(EDTAB01L) and
137000130419     C                             TABFLG = *BLANKS
137100130419     C                   MOVEL     TABUNI        edidsPO
137200130419     C                   MOVEL     TABKEY        PO_key
137300130419     C                   Z-ADD     1             X
137400130419     C     PO_key        LOOKUP    PT_key(X)                              32
137500130419     C   32              MOVEL     TABUNI        PO_uni(X)
137600130419     C                   END
137700130419      *
137800130415     C* Caricamento Tabella codici clienti - tariffe
137900130415     C                   Z-ADD     0             X
138000130415     C                   MOVEL     'CL'          TABCOD
138100130415     C     TABCOD        CHAIN     EDTAB01L                           30
138200130415     C     *IN30         DOWEQ     '0'
138300130415     C                   if        TABFLG = *BLANKS
138400130415     C                   ADD       1             X
138500130415     C                   MOVEL     TABKEY        Cod_Tb_CL(X)
138600130415     C                   MOVEL     TABUNI        Des_Tb_CL(X)
138700130415     C                   END
138800130415     C     TABCOD        READE     EDTAB01L                               30
138900130415     C                   END
139000130415      *
139100130415     C* Caricamento Serivizi speciali
139200130415     C                   Z-ADD     0             X
139300130415     C                   MOVEL     'SS'          TABCOD
139400130415     C     TABCOD        CHAIN     EDTAB01L                           30
139500130415     C     *IN30         DOWEQ     '0'
139600130415     C                   if        TABFLG = *BLANKS
139700130415     C                   ADD       1             X
139800130415     C                   eval       Key_ServSpec(X) = TABKEY
139900130415     C                   eval       SpecialServ(X)  = TABKEY
140000130415     C                   eval       UNI_ServSpec(X) = TABUNI
140100130415     C                   END
140200130415     C     TABCOD        READE     EDTAB01L                               30
140300130415     C                   END
140400130416      *
140500130416     C* Caricamento Tabella 15
140600130416      * Nazione di consegna x P.O. di Consegna ed Emittente
140700130416     C                   z-add     0             Nz                3 0
140800130416     c                   clear                   Naz03
140900130416     c                   clear                   Iso03
141000130416     c                   clear                   Cpf03
141100130418     C                   z-add     1             tblKUT
141200130418     C                   MOVEL(p)  '15'          tblCOD
141300130418     C     KTABel        setll     TABEL00F
141400130418     C     KTABel        reade     TABEL00F
141500130416DO  1C                   Dow       not %EoF(TABEL00F)
141600130416DO  1C                   if         tblflg = *blank
141700130416     C                   MOVEL     TBLuni        DS15
141800130416     C                   add       1             Nz
141900130416     C                   MOVEL     tblKEY        Naz03(Nz)
142000130416     C                   MOVEL     §15COD        Iso03(Nz)
142100130416     C                   MOVEL     §15ECF        CPF03(Nz)
142200130416     C                   MOVE      §15PCF        CPF03(Nz)
142300130416E   1C                   End
142400130418     C     KTABel        reade     TABEL00F
142500130416E   1C                   End
142600130416      *
142700130416     C* Caricamento Tabella Priorità trasporto
142800130416     C                   Z-ADD     0             X                 4 0
142900130416     C                   MOVEL     'TS'          TABCOD
143000130416     C     TABCOD        CHAIN     EDTAB01L                           30
143100130416     C     *IN30         DOWEQ     '0'
143200130416DO  1C                   if         tblflg = *blank
143300130416     C                   ADD       1             X
143400130416     C                   eval      TipoServizio(X) = TABUNI
143500130416     C                   eval      Key_TipServ(X)  = TABKEY
143600130416     C                   END
143700130416     C     TABCOD        READE     EDTAB01L                               30
143800130416     C                   END
143900130415      *
144000130415     C                   ENDSR
144100130416     c*==================================================================*
144200130416      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
144300130416     c*==================================================================*
144400130416     c     Imp_ADDR_mail begsr
144500130416      *
144600130416     c                   clear                   Indirizzi
144700130416      *
144800130416      *  Lunghezza indirizzi presenti
144900130416     c                   eval      Lunghezza_Indirizzo =
145000130416     c                                       %Len(%TrimR(Mail_Ind))
145100130416     c                   z-add     1             al_char           3 0
145200130416     c                   z-add     1             dal_char          3 0
145300130416     c                   z-add     1             tot_char          3 0
145400130416      *
145500130416     c     successivo    tag
145600130416      *
145700130416      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
145800130416      *   e aggiunge il dominio Brt + il (;) separatore
145900130416     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
146000130416     c                   eval      tot_char = al_char - dal_char
146100130416     c                   z-add     dal_char      dalCarattere
146200130416     c                   z-add     tot_char      xTOTcaratteri
146300130416      *
146400130416     c                   clear                   Indir_BRT        40
146500130416     c                   clear                   Indir_Parz       20
146600130416     c                   eval      Indir_Parz =
146700130416     c                             %Subst(Mail_Ind:dalCarattere:xTOTcaratteri)
146800130416      *
146900130416     c                   eval      Indir_BRT    = %Trim(Indir_Parz) +
147000130416     c                             %Trim(bart_it)
147100130416     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_BRT
147200130416      *
147300130416     c                   if        al_char = Lunghezza_Indirizzo
147400130416     c                   goto      fine_indmail
147500130416     c                   else
147600130416     c                   eval      dal_char = ( al_char + 1 )
147700130416     c                   goto      successivo
147800130416     c                   end
147900130416      *
148000130416     C     fine_indmail  TAG
148100130416      *
148200130416     C                   ENDSR
148300130416     c*==================================================================*
148400130416      * Manda un Msg x E-mail
148500130416     c*==================================================================*
148600130416     c     Invio_Mail    begsr
148700130416      *
148800130416     C*   Alert_mail : invio Mail a CED@Brt.it                  *
148900130416     C* Inizializzo variabili
149000130416     C                   movel     *blanks       wrkEml          253
149100130416     C                   movel     *blanks       wrkMsg         5000
149200130416     C                   movel     *blanks       wrkOgg           44
149300130416      *
149400130416     C* Valorizzo i campi della e-m@ail - indirizzo
149500130416     C*******            eval      wrkEml = 'Andrea.Bertocchi@Brt.it'
149600130416     C                   eval      wrkEml = Indirizzi
149700130416     C                   eval      wrkOgg = Oggetto
149800130416     C                   eval      wrkMsg = Messaggio
149900130416      ** *****
150000130416     C                   call(e)   'TRTCT00R2'
150100130416     C                   parm                    wrkEml
150200130416     C                   parm                    wrkOgg
150300130416     C                   parm                    wrkMsg
150400130416     C*
150500130416     C                   ENDSR
150600130415      * ?------------------------------------------------------------------ */
150700130502     C* Solo la prima volta  aprendo in RT
150800130502      * ?------------------------------------------------------------------ */
150900130502     C     *inzsr        begSR
151000130502     C*
151100130502     C                   ENDSR
151200130502      * ?------------------------------------------------------------------ */
