000100000000     h*--------------------------------------------------------------------------------------------*
000200170125     h* Trascodifica dati RPVP da FLTR5 a FITR4 (trak VDL)                                         *
000300000000     h*--------------------------------------------------------------------------------------------*
000400000000     h DECEDIT('0,') DATEDIT(*DMY.)
000500000000     f*--------------------------------------------------------------------------------------------*
000600000000     f* Data base
000700000000     f*--------------------------------------------------------------------------------------------*
000800030716     fazorg01l  IF   E           K DISK
000900091125     ffltr5err  O  A E             DISK    prefix(e_)
001000030716     ffltr500f  UF   E             DISK
001100170125     ffitr400f  O  A E             DISK
001200000000     d*--------------------------------------------------------------------------------------------*
001300000000     d* Data structure
001400000000     d*--------------------------------------------------------------------------------------------*
001500000000     d*---
001600030716     d* Variabili di wrk
001700000000     d*---
001800030716     d flgOK           S              1
001900030716     d wPOn            S              3  0
002000030716     d wPOa            S              3
002100030716     d wBarcode        S             20
002200030718     d wVolume         S              7  0
002300030718     d wLatoCub        S             30  9
002400030721     d wLatoCubInt     S              4  0
002500030721     d wLatoCubIntA    S              4
002600030716     d wArrotonda1a    S              1
002700030716     d wArrotonda1n    S              1  0
002800030716     d wArrotonda2a    S              4
002900030716     d wArrotonda2n    S              4  0
003000000000     d*---
003100030716     d* DS ridefinizione file piatto d input da pc RPVP
003200000000     d*---
003300030716     d RPVP_DS         DS
003400030716     d  RPVP_TipApp                   1
003500030716     d  RPVP_ID                      30
003600030716     d  RPVP_PesoGr                   7
003700030716     d  RPVP_VolCm3                   7
003800030716     d  RPVP_LungMm                   5
003900030716     d  RPVP_AltezzMm                 5
004000030716     d  RPVP_ProfMm                   5
004100030716     d  RPVP_DataOra                 14
004200170207     d  RPVP_VolOK                    4
004300170207     d  RPVP_PesOK                    4
004400030716     d*---
004500030716     d* DS ridefinizione file piatto d output
004600030716     d*---
004700170125     d fitr4DA1_DS     DS
004800030716     d  tr4ctl                 1      1
004900030716     d  tr4npl                 2      3
005000030716     d  tr4nfv                 4      8
005100030716     d  tr4tim                 9     22
005200030716     d  tr4ms1                23     27
005300030716     d  tr4msx                28     28
005400030716     d  tr4ms2                29     31
005500030716     d  tr4cod                32     36
005600030716     d  tr4020                37     38
005700030716     d  tr4f01                39     39
005800030716     d  tr4pic                40     44
005900030716     d  tr4f02                45     45
006000030716     d  tr4pgr                46     51
006100030716     d  tr4f03                52     52
006200030716     d  tr4pod                53     55
006300030716     d  tr4f04                56     56
006400030716     d  tr4ods                57     57
006500030716     d  tr4f05                58     58
006600030716     d  tr4pad                59     61
006700030716     d  tr4f06                62     62
006800030716     d  tr4dts                63     63
006900030716     d  tr4f07                64     64
007000030716     d  tr4ps1                65     65
007100030716     d  tr4pn1                66     67
007200030716     d  tr4pd1                68     97
007300030716     d  tr4f08                98     98
007400030716     d  tr4ps2                99     99
007500030716     d  tr4pn2               100    101
007600030716     d  tr4pd2               102    130
007700030716     d  tr4xxx               131    131
007800030716     d  tr4f09               132    132
007900030716     d  tr4ps3               133    133
008000030716     d  tr4pn3               134    135
008100030716     d  tr4pd3               136    165
008200030716     d  tr4f10               166    166
008300030716     d  tr4ps4               167    167
008400030716     d  tr4pn4               168    169
008500030716     d  tr4pd4               170    199
008600030716     d  tr4dm1               181    184
008700030716     d  tr4dm2               185    188
008800030716     d  tr4dm3               189    192
008900030716     d  tr4f11               200    200
009000030716     d  tr4pep               201    214
009100030716     d  tr4f12               215    215
009200030716     d  tr4pes               216    216
009300030716     d  tr4f13               217    217
009400030716     d  tr4pup               218    231
009500030716     d  tr4f14               232    232
009600030716     d  tr4pus               233    233
009700030716     d  tr4f15               234    234
009800030716     d  tr4crc               235    236
009900030716     d*---
010000030716     d* Parametri in entrata
010100030716     d*---
010200030716     d  IN_Fil         S              4                                          - filiale
010300030716     d  IN_TFV         S              2                                          - tipo FV
010400030716     d  IN_NFV         S              7                                          - numero FV
010500000000     c*--------------------------------------------------------------------------------------------*
010600000000     c* Main lines
010700000000     c*--------------------------------------------------------------------------------------------*
010800000000     c*
010900030716     c* Leggo tutto il file d input
011000030716     c                   READ      fltr500f
011100030716     c                   DOW       not %eof(fltr500f)
011200091125     c*
011300091125     c* Verifica propedeutica => verifica correttezza presunta formato record dati input
011400091125     c                   if        %len(%trim(tr5dati)) > 83
011500091125     c                   eval      E_TR5KEY  = IN_Fil
011600091125     c                   eval      E_TR5DATI = TR5DATI
011700091125     c                   write     fltr500e
011800091125     c                   delete    fltr5000
011900091125     c                   exsr      sndeml
012000091125     c                   else
012100030716     c*
012200030716     c* Inizializzo il flag del controllo barcode d tipo Bartolini
012300030716     c                   EVAL      flgOK = 'S'
012400030716     c*
012500170125     c* Effettuo trascodifica del FLTR5 e scrivo il FITR4
012600030716     c                   EVAL      RPVP_DS = tr5dati
012700040218     c*
012800040218     c* Verifico subito se è passato il codice tipo applicazione dal file generato sul pc RPVP
012900040220     c                   IF        (RPVP_TipApp =  'C'  OR
013000040220     c                              RPVP_TipApp =  'K'  OR
013100040220     c                              RPVP_TipApp =  'S'  OR
013200040220     c                              RPVP_TipApp =  'A') AND
013300040220     c                              RPVP_ID     <> *blanks
013400170125     c*
013500170125     c* Inizializzo dati di wrk e di output
013600170125     c                   CLEAR                   fitr4
013700170125     c                   CLEAR                   fitr4DA1_DS
013800170125     c*
013900170125     c* Compongo la struttura di DATI
014000030716     c                   EVAL      tr4ctl = %subst(IN_TFV:2:1)
014100030716     c                   EVAL      tr4npl = *zeros
014200030716     c                   EVAL      tr4nfv = %subst(IN_NFV:3:5)
014300030716     c                   EVAL      tr4tim = RPVP_DataOra
014400030716     c                   EVAL      tr4ms1 = *zeros
014500030716     c                   EVAL      tr4msx = *blanks
014600030716     c                   EVAL      tr4ms2 = *blanks
014700030716     c                   EVAL      tr4cod = *zeros
014800030716     c                   EVAL      tr4f01 = *blanks
014900030716     c                   EVAL      tr4pic = *zeros
015000030716     c                   EVAL      tr4f02 = *blanks
015100071011     c                   EVAL      tr4pgr = %subst(RPVP_PesoGr:1:6)
015200030716     c                   EVAL      tr4f03 = *blanks
015300030716     c                   EVAL      tr4pod = '001'
015400030716     c                   EVAL      tr4f04 = *blanks
015500030716     c                   EVAL      tr4f05 = *blanks
015600030716     c                   EVAL      tr4pad = '001'
015700030716     c                   EVAL      tr4f06 = *blanks
015800030716     c                   EVAL      tr4dts = *zeros
015900030716     c                   EVAL      tr4f07 = *blanks
016000030716     c                   EVAL      tr4ps1 = *zeros
016100030716     c                   EVAL      tr4pn1 = *zeros
016200170125     c*
016300030716     c* Verifica se trattasi di barcode Bartolini
016400030716     c* - verifico se è numerico
016500030716     c                   IF        flgOK = 'S'
016600030716     c                   EVAL      PiStr = %trim(RPVP_ID)
016700030716     c                   EXSR      CHKNUM
016800030716     c                   IF        PiInt=*on
016900030716     c                   SETON                                        08
017000030716     c                   ELSE
017100030716     c                   SETOFF                                       08
017200030716     c                   EVAL      flgOK = 'N'
017300030716     c                   ENDIF
017400030716     c                   ENDIF
017500170125     c*
017600030716     c* - verifico se lunghezza è 18
017700030716     c                   IF        flgOK = 'S'
017800030716     c                   IF        %len(%trim(RPVP_ID)) = 18
017900030716     c                   SETON                                        09
018000030716     c                   ELSE
018100030716     c                   SETOFF                                       09
018200030716     c                   EVAL      flgOK = 'N'
018300030716     c                   ENDIF
018400030716     c                   ENDIF
018500170125     c*
018600030716     c* - controllo la LNP
018700030716     c                   IF        flgOK = 'S'
018800030716     c                   EVAL      wPOa = %subst(%trim(RPVP_ID):1:3)
018900030716     c                   MOVE(P)   wPOa          wPOn
019000030716     c     wPOn          CHAIN     azorg01l
019100030716     c                   IF        %found(azorg01l)
019200030716     c                   SETON                                        10
019300030716     c                   ELSE
019400030716     c                   SETOFF                                       10
019500030716     c                   EVAL      flgOK = 'N'
019600030716     c                   ENDIF
019700030716     c                   ENDIF
019800170125     c*
019900030716     c* - controllo la LNA
020000030716     c                   IF        flgOK = 'S'
020100030716     c                   EVAL      wPOa = %subst(%trim(RPVP_ID):4:3)
020200030716     c                   MOVE(P)   wPOa          wPOn
020300030716     c     wPOn          CHAIN     azorg01l
020400030716     c                   IF        %found(azorg01l)
020500030716     c                   SETON                                        11
020600030716     c                   ELSE
020700030716     c                   SETOFF                                       11
020800030716     c                   EVAL      flgOK = 'N'
020900030716     c                   ENDIF
021000030716     c                   ENDIF
021100170207     c*
021200170207     c* Verifoco se rilevazioni da volumatore sono corrette
021300170207     c                   IF        flgOK = 'S'
021400170207     c                   IF        RPVP_VolOK = '0000'
021500170207     c                   SETON                                        12
021600170207     c                   ELSE
021700170207     c                   SETOFF                                       12
021800170207     c                   EVAL      flgOK = 'N'
021900170207     c                   ENDIF
022000170207     c                   ENDIF
022100170207     c*
022200170207     c* Verifoco se rilevazioni da volumatore sono corrette
022300170207     c                   IF        flgOK = 'S'
022400170207     c                   IF        RPVP_PesOK = '0000'
022500170207     c                   SETON                                        13
022600170207     c                   ELSE
022700170207     c                   SETOFF                                       13
022800170207     c                   EVAL      flgOK = 'N'
022900170207     c                   ENDIF
023000170207     c                   ENDIF
023100170125     c*
023200170125     c* ...quindi se segnacolo Bartolini e tipo applicazione K o C:
023300030716     c                   IF        *IN08 = *on AND
023400030716     c                             *IN09 = *on AND
023500030716     c                             *IN10 = *on AND
023600030716     c                             *IN11 = *on AND
023700170207     c                             *IN12 = *on AND
023800170207     c                             *IN13 = *on AND
023900031212     c                             (RPVP_TipApp = 'K'  or
024000031212     c                              RPVP_TipApp = 'C')
024100030716     c                   EVAL      wBarcode = '#' + %trim(RPVP_ID) + '#'
024200030716     c                   MOVE(P)   wBarcode      tr4pd1
024300030716     c                   ELSE
024400030716     c                   EVAL      wBarcode = '#' + '??????????????????' + '#'
024500030716     c                   MOVE(P)   wBarcode      tr4pd1
024600030716     c                   ENDIF
024700030716     c*
024800030716     c                   EVAL      tr4f08 = *blanks
024900030716     c                   EVAL      tr4ps2 = '6'
025000030716     c                   EVAL      tr4pn2 = '00'
025100170125     c*
025200031205     c* Se segnacolo Bartolini e tipo applicazione K o C:
025300030716     c                   IF        *IN08 = *on AND
025400030716     c                             *IN09 = *on AND
025500030716     c                             *IN10 = *on AND
025600030716     c                             *IN11 = *on AND
025700170207     c                             *IN12 = *on AND
025800170207     c                             *IN13 = *on AND
025900031212     c                             (RPVP_TipApp = 'K'  or
026000031212     c                              RPVP_TipApp = 'C')
026100030716     c                   EVAL      tr4pd2 = *blanks
026200030716     c                   ELSE
026300030722     c                   EVAL      tr4pd2 = *all'?'
026400030722     c                   EVAL      %subst(tr4pd2:1:1) = '#'
026500030722     c                   EVAL      %subst(tr4pd2:(%len(tr4pd2)-
026600030722     c                             %len(%trim(RPVP_ID))+1):%len(%trim(RPVP_ID)))
026700030722     c                             = %trim(RPVP_ID)
026800030716     c                   ENDIF
026900030716     c*
027000030716     c                   EVAL      tr4f09 = *blanks
027100030716     c                   EVAL      tr4ps3 = *zeros
027200030716     c                   EVAL      tr4pn3 = *zeros
027300071127     c***                EVAL      tr4pd3 = *zeros
027400071127     c                   EVAL      tr4pd3 = %subst(RPVP_PesoGr:1:6)
027500030716     c                   EVAL      tr4f10 = *blanks
027600030716     c                   EVAL      tr4ps4 = *zeros
027700030716     c                   EVAL      tr4pn4 = *zeros
027800170125     c*
027900170125     c* Eseguo alcune considerazioni x la valorizzazione (troncata) delle misure
028000030718     c*
028100030718     c* Dal volume reperisco il lato cubico approssimato
028200030721     c                   MOVE(P)   RPVP_VolCm3   wVolume
028300030721     c                   EVAL      wLatoCub = wVolume**(1/3)
028400071011     c                   EVAL      wLatoCub = wLatoCub * 10
028500030721     c                   EVAL      wLatoCubInt = %inth(wLatoCub)
028600031215     c                   MOVE(P)   wLatoCubInt   wLatoCubIntA
028700030721     c* - lunghezza
028800030721     c                   MOVE(P)   wLatoCubIntA  tr4dm1
028900030716     c* - altezza
029000030721     c                   MOVE(P)   wLatoCubIntA  tr4dm2
029100030716     c* - profondità
029200030721     c                   MOVE(P)   wLatoCubIntA  tr4dm3
029300030716     c*
029400030716     c                   EVAL      tr4f11 = *blanks
029500170125     c*
029600031205     c* Imposto baia entrata e tipo record
029700031205     c                   SELECT
029800031205     C                   when      RPVP_TipApp = 'C'
029900031205     c                   MOVE(P)   'RPVP_C'      tr4pep
030000031205     c                   EVAL      tr4020 = '2C'
030100031205     c                   when      RPVP_TipApp = 'K'
030200030722     c                   MOVE(P)   'RPVP_K'      tr4pep
030300031205     c                   EVAL      tr4020 = '2K'
030400031205     c                   when      RPVP_TipApp = 'S'
030500030722     c                   MOVE(P)   'RPVP_S'      tr4pep
030600031205     c                   EVAL      tr4020 = '22'
030700031205     c                   when      RPVP_TipApp = 'A'
030800031205     c                   MOVE(P)   'RPVP_A'      tr4pep
030900031205     c                   EVAL      tr4020 = '22'
031000031205     c                   other
031100031205     c                   MOVE(P)   'RPVP_ERR'    tr4pep
031200031205     c                   EVAL      tr4020 = '22'
031300031205     c                   ENDSL
031400030722     c*
031500030716     c                   EVAL      tr4f12 = *blanks
031600030716     c                   EVAL      tr4pes = '1'
031700030716     c                   EVAL      tr4f13 = *blanks
031800030716     c                   EVAL      tr4pup = *blanks
031900030716     c                   EVAL      tr4f14 = *blanks
032000170125     c*
032100031205     c* Se segnacolo Bartolini e tipo applicazione K o C:
032200030716     c                   IF        *IN08 = *on AND
032300030716     c                             *IN09 = *on AND
032400030716     c                             *IN10 = *on AND
032500030725     c                             *IN11 = *on AND
032600170207     c                             *IN12 = *on AND
032700170207     c                             *IN13 = *on AND
032800031212     c                             (RPVP_TipApp = 'K'  or
032900031212     c                              RPVP_TipApp = 'C')
033000030716     c                   EVAL      tr4pus = '1'
033100030716     c                   ELSE
033200030716     c                   EVAL      tr4pus = '0'
033300030716     c                   ENDIF
033400030716     c*
033500030725     c                   IF        tr4pus = '0'
033600030725     c                   EVAL      tr4ods = '3'
033700030725     c                   ELSE
033800030725     c                   EVAL      tr4ods = '1'
033900030725     c                   ENDIF
034000030725     c*
034100030716     c                   EVAL      tr4f15 = *blanks
034200030716     c                   EVAL      tr4crc = *blanks
034300030716     c*
034400170125     c* Al termine della trascodifica scrivo il FITR4 e cancello il record d input dal FLTR5
034500170125     c                   eval      tr4fil    = %subst(IN_Fil:2:3)
034600170125     c                   eval      tr4pc     = *blanks
034700170125     c                   eval      tr4da1    = fitr4DA1_DS
034800170125     c                   eval      tr4datora = %editc(%dec(%date():*ISO):'X')+
034900170125     c                                         %editc(%dec(%time():*ISO):'X')
035000170125     c                   write     fitr4
035100030716     c                   delete    fltr5000
035200040220     c                   ENDIF
035300091125     c*
035400091125     c                   endif
035500030716     c*
035600030716     c* Proseguo con la lettura
035700030716     c                   READ      fltr500f
035800030716     c                   ENDDO
035900000000     c*
036000030716     c                   SETON                                        LR
036100170125
036200170125
036300170125
036400030716     c*--------------------------------------------------------------------------------------------*
036500030716     c*  CONTROLLO NUMERICITA'
036600030716     c*--------------------------------------------------------------------------------------------*
036700030716     c     CHKNUM        BEGSR
036800030716     c*
036900030716     c                   IF        PiDecChr = *blanks
037000030716     c                   EVAL      PiDecChr = ','
037100030716     c                   ENDIF
037200030716     c*
037300030716     c                   CALL(e)   'ISNUMERIC'
037400030716     c                   PARM                    PiStr            30
037500030716     c                   PARM                    PiDecChr          1
037600030716     c                   PARM      *ZEROS        PiVal            30 9
037700030716     c                   PARM      '0'           PiInt             1
037800030716     c                   PARM      '0'           PiNum             1
037900030716     c                   IF        %error
038000030716     c                   EVAL      PiNum=*off
038100030716     c                   ENDIF
038200030716     c*
038300030716     c                   ENDSR
038400170125
038500170125
038600091125
038700091125     c*--------------------------------------------------------------------------------------------*
038800120302     c* /TITLE Compongo il testo e spedisco una @-mail
038900091125     c*--------------------------------------------------------------------------------------------*
039000091125     c     sndeml        begsr
039100091125     c*
039200091125     c* Inizializzo variabili
039300091125     c                   movel     *blanks       wrkEml          253
039400091125     c                   movel     *blanks       wrkEmlMsg      5000
039500091125     c                   movel     *blanks       wrkEmlOgg        44
039600120302     c* Valorizzo i campi della @-mail
039700120302     c                   eval      wrkEml='cedalert@brt.it '
039800091125     c                   eval      wrkEmlOgg='SPUNTE DA RPVP - ATTENZIONE !!!'
039900091125     c                   EVAL      wrkEmlMsg = 'Presenti errori formali nei '  +
040000091125     c                             'record spunta da RPVP per la filiale '     +
040100091125     c                             %subst(IN_Fil:2:3) + '.'              +':/N'+
040200091125     c                                                                    ':/N'+
040300091125     c                             'Verificare i dati errati nel file FLTR5ERR'+
040400091125     c                             ' con chiave: ' + IN_Fil + '.'
040500091125     c*
040600091125     c                   call(e)   'TIS701C'
040700091125     c                   parm                    wrkEml
040800091125     c                   parm                    wrkEmlOgg
040900091125     c                   parm                    wrkEmlMsg
041000091125     c*
041100091125     c                   endsr
041200170125
041300170125
041400091125
041500000000     c*--------------------------------------------------------------------------------------------*
041600030716     c* Esegue operazioni iniziali
041700030716     c*--------------------------------------------------------------------------------------------*
041800030716     c     *inzsr        BEGSR
041900030716     c*
042000030716     c* Ricezione parametri in entrata
042100030716     c     *ENTRY        PLIST
042200030716     c                   PARM                    IN_Fil
042300030716     c                   PARM                    IN_TFV
042400030716     c                   PARM                    IN_NFV
042500030716     c*
042600030716     c                   ENDSR
