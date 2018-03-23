000100101220       //==============================================================
000200101220       //?  STORE PROCEDURE - DATI PER IMMAGINI LDV (TERMINALINI AUT)  ?
000300130411       //?                    (ANCHE RIGENERAZIONE)                    ?
000400170307       //==============================================================
000500170307       //?                     COPIA DI  "FISP30R"                     ?
000600101220       //==============================================================
000700101220       //
000800101220       //? QUESTO PROGRAMMA È "LEGATO"? A "FNLSB6R" (STAMPA LDV A4).  ?
000900130214       //? VERIFICARE SE LA MODIFICA E' DA APPORTARE ANCHE AD ESSO !!  ?
001000101220       //
001100130924       //ATTENZIONE:?I CAMPI NEL RECORD LSB6LDV RISULTANO IMPOSTATI
001200101220       //             NELLE VARIE STRUTTURE DATI FISP30nnDS - TUTTI
001300101220       //             ALFANUMERICI !
001400130924       //
001500130924       //NOTA BENE (a):?Valori previsti nel parm di output RISULTATO:?
001600130924       //               ?· 1) "OK" => Generare l'immagine             ?
001700130924       //               ?· 2) "NO" => Ripetere la richiesta           ?
001800130924       //               ?· 3) "XX" => NON ripetere la richiesta       ?
001900101220       //
002000130924       //NOTA BENE (b):?> Cod. cliente 0501331 => FedEx
002100150430       //                  cod. fissato nella costante "c_CliFedEx"
002200150430       //                > Codice cliente 0506514 => FedEx (2)
002300150430       //                  codice fissato nella costante "c_CliFedEx2"
002400130924       //                > Cod. cliente 0201515 => PraOil oleodotti
002500150430       //                  cod. fissato nella costante "c_CliPraOil"
002600130924       //                > Cod. cliente 0504450 => TTL
002700130924       //                  cod. fissato nella costante "c_CliTTL"
002800130924       //                > Cod. cliente 0632910 => Consorzio Lotterie Naz
002900130924       //                  cod. fissato nella costante "c_CliCLN"
003000140513       //                > Cod. cliente 0017093 => Lotterie Nazionali
003100140513       //                       (gemello di backup su Bologna)
003200140513       //                  cod. fissato nella costante "c_CliLN_2"
003300130924       //                > Cod. cliente 0634923 => S G Soluzioni Logist.
003400130924       //                  cod. fissato nella costante "c_CliSG "
003500130924       //                > Cod. cliente 0635042 => S G Soluzioni Logist.
003600130924       //                  cod. fissato nella costante "c_CliSG_2"
003700101220       //
003800101220       //==============================================================
003900101213
004000101213       //--------------------------------------------------------------
004100101213       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
004200101213       //--------------------------------------------------------------
004300101213
004400130411     /*PRM  dbgview(*source)
004500130411     /*PRM  commit(*NONE)
004600130411     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
004700130411     /*CMD         ovrscope(*calllvl)
004800130411     /*END  dltovr file(TITAS30C) lvl(*)
004900101213     /*END
005000101213
005100101213       //--------------------------------------------------------------
005200101213       //?Specifiche di controllo.                                     ?
005300101213       //--------------------------------------------------------------
005400080430
005500080430     h decedit('0,') datedit(*dmy/) option(*nodebugio)
005600080718     h dftactgrp(*no) actgrp(*caller)
005700080430
005800101213       //--------------------------------------------------------------
005900101213       //?Dichiarazione file.                                          ?
006000101213       //--------------------------------------------------------------
006100080430
006200101213       // -?Tabelle?
006300101213     fTABEL00F  if   e           k disk    usropn
006400101213
006500101213       // -?Organigramma di filiale?
006600101213     fAZORG01L  if   e           k disk    usropn
006700130411
006800130411       // -?Distinte delle cui LdV ri-generare le immagini?
006900130422     fFIIRD11L  if   e           k disk
007000130411
007100130411       // -?Dati per LdV (sede)?
007200130411     fTITAS30C  if   e           k disk    usropn
007300130411     f                                     extfile(wLibFileTITAS)
007400101213
007500101213       // -?Dati per LdV arrivo?
007600070305     fFNARB01L  if   e           k disk    usropn
007700070306     fFNART01L  if   e           k disk    usropn
007800131206     fFNEVB04L  if   e           k disk    usropn
007900140303     fFNEVB22L  if   e           k disk    usropn
008000080710     fFIDST01L  if   e           k disk    usropn
008100101213
008200101213       // -?Altri dati comuni?
008300090619     fFNLBL01L  if   e           k disk    usropn
008400081030     fFIARS01L  if   e           k disk    usropn
008500070305     fFIAR401L  if   e           k disk    usropn
008600030515     fFIAR501L  if   e           k disk    usropn
008700070305     fFIAR601L  if   e           k disk    usropn
008800070305     fFIAR701L  if   e           k disk    usropn
008900070305     fFIAR901L  if   e           k disk    usropn
009000110117
009100131017       // -?Dati per LdV partenza (solo per bolla mamma - cliente CLN)?
009200110117     fFNBLP01L  if   e           k disk    usropn
009300101213
009400101207       // -?O.R.M.?
009500100517     fFNORM01L  if   e           k disk    usropn
009600101213
009700110309       // -?File LOG spedizioni elaborate per immagine LdV?
009800110202     fFIILV01L  Uf A e           k disk    usropn
009900130227
010000130227       // -?Spool da inviare via e-mail:?
010100130227       //   ·?Corpo del messaggio di posta elettronica?
010200130227     fPrtEMAIL  o    f  132        printer usropn  oflind(*inOE)
010300101213
010400101207       // -?Printer File  (se richiesta bozza in stampa)?
010500090210     fQSYSPRT   o    f  132        printer usropn  oflind(*inOF)
010600030515
010700101213       //--------------------------------------------------------------
010800101213       //?Definizione costanti.                                        ?
010900101213       //--------------------------------------------------------------
011000030515
011100110117     d c_DateMax_A     c                   const('20391231')
011200131008       // -?Costanti in output?
011300110117     d c_ScNS          c                   const('ELENCO SEGNACOLLI -
011400030515     d                                     NON SEQUENZIALI')
011500110117     d c_CoOr          c                   const('COLLI ORIGINALI     -
011600030717     d                                     N. ')
011700110117     d c_PMxA          c                   const('PROMEMORIA PER L''AU-
011800070315     d                                     TOTRASPORTATORE')
011900101213       // -?Codice cliente "FedEx"?
012000150430     d c_CliFedEx      c                   const(0501331)
012100150430     d c_CliFedEx2     c                   const(0506514)
012200101213       // -?Codice cliente "PraOil oleodotti italiani"?
012300150430     d c_CliPraOil     c                   const(0201515)
012400101213       // -?Codice cliente "TTL"?
012500091116     d c_CliTTL        c                   const(0504450)
012600101213       // -?Codice cliente "Lotterie Nazionali - Gratta & Vinci"?
012700090619     d c_CliCLN        c                   const(0632910)
012800140513     d c_CliLN_2       c                   const(0017093)
012900101213       // -?Codice cliente "S G Soluzioni Logistiche"?
013000090619     d c_CliSG         c                   const(0634923)
013100091117     d c_CliSG_2       c                   const(0635042)
013200110923
013300110923       // -?Consegna CON Riserva specifica?
013400110923     d c_RiservaSpecif...
013500110923     d                 c                   const('RS')
013600091117
013700131008       // -?Costante per evitare l'indicazione del contratto x "S.G."?
013800091117     d c_NoContr       c                   const('NO CONTRATTO')
013900101213
014000101213       // -?Costante per controllo "caratteri solo numerici"?
014100110126     d c_Digits        c                   const('0123456789')
014200101207       // -?Numero di righe da gestire per la LdV?
014300130213     d c_Row           c                   const(36)
014400081027     d c_Row_B         c                   const(49)
014500130213     d c_Row_B2        c                   const(51)                            c_Row_B + 2
014600081030     d c_Row_Fed       c                   const(10)
014700101207       // -?Sequenza numerica?
014800080430     d c_Char          c                   const('*...+....1....+....2+
014900080430     d                                            ....+....3....+....4+
015000080430     d                                            ....+....5....+....6+
015100080430     d                                            ....+....7....+....8+
015200080430     d                                            ....+....9....+...10+
015300080430     d                                            ....+...11....+...12')
015400130227
015500130227       // -?Dati di default?
015600130227     d c_Mittente      c                   const('ced')
015700130227     d c_at            c                   const('@brt.it')
015800130227     d c_Sede          c                   const('046')
015900130227
016000130227       // -?Comandi di override al PrtF "corpo e-mail"?
016100130227     d c_CmdOvrPrtF    c                   const('OVRPRTF +
016200130227     d                                           file(PRTEMAIL) +
016300130227     d                                           pagesize(66 132) +
016400130227     d                                           lpi(6) cpi(10) +
016500130227     d                                           ovrscope(*actgrpdfn) +
016600130227     d                                           ')
016700130227     d c_CmdDltOvr     c                   const('DLTOVR +
016800130227     d                                            file(PRTEMAIL) +
016900130227     d                                            lvl(*actgrpdfn)')
017000101213
017100101213       //--------------------------------------------------------------
017200101213       //?Definizione schiere.                                         ?
017300101213       //--------------------------------------------------------------
017400101213
017500101213       // -?Giorni della settimana e relativa decodifica?
017600070306     d $GgS            s              1    dim(9)   ctdata  perrcd(1)
017700070306     d $Gio            s             15    dim(9)   alt($GgS)
017800110202
017900110202       // -?Codice / Decodifica LOG x FIILV00F?
018000110202     d $CoL            s                   like(ILVcol)
018100130313     d                                     dim(10)  ctdata  perrcd(1)
018200110202     d $DeL            s                   like(ILVnot)
018300130313     d                                     dim(10)  alt($CoL)
018400101213
018500101207       // -?Dati da restituire?
018600130214     d LdV             ds           123    occurs(c_Row_B2) inz
018700080508     d   LdvPrg                       3s 0 inz
018800080430     d   LdvTxt                     120    inz
018900101213
019000101213       // -?Note e Particolarità?
019100070306     d $NP             s                   dim(6)   inz  like(Pnp1)
019200101213       // -?Consegne Precedenti?
019300070319     d $CP             s                   dim(20)  inz(*all'9')
019400070319     d                                                   like(Elem_CP)
019500070319     d $CPdcm          s                   dim(20)  inz  like(ARTdcm)
019600101213
019700101213       //--------------------------------------------------------------
019800101213       //?Definizione strutture dati.                                  ?
019900101213       //--------------------------------------------------------------
020000101213
020100080430     d KPJBA         e ds                  inz
020200101213
020300131008       // -?Dati LDV da restituire per l'immagine?
020400080506     d FNLSB5ds      e ds                  inz
020500080430
020600110202       // -?Records in output?
020700110202       // ·? 1ª riga?
020800081021     d ds_01         e ds                  inz  extname(FISP3001ds)
020900081021     d                                          prefix(P : 3)
021000110202       // ·? 2ª riga (vuota)?
021100110202       // ·? 3ª riga?
021200080430     d ds_03         e ds                  inz  extname(FISP3003ds)
021300080430     d                                          prefix(P : 3)
021400110202       // ·? 4ª riga?
021500080430     d ds_04         e ds                  inz  extname(FISP3004ds)
021600080430     d                                          prefix(P : 3)
021700110202       // ·? 5ª riga?
021800080430     d ds_05         e ds                  inz  extname(FISP3005ds)
021900080430     d                                          prefix(P : 3)
022000110202       // ·? 6ª riga (vuota)?
022100110202       // ·? 7ª riga?
022200080430     d ds_07         e ds                  inz  extname(FISP3007ds)
022300080430     d                                          prefix(P : 3)
022400110202       // ·? 8ª riga (vuota)?
022500110202       // ·? 9ª riga?
022600080430     d ds_09         e ds                  inz  extname(FISP3009ds)
022700080430     d                                          prefix(P : 3)
022800110202       // ·?10ª riga?
022900080430     d ds_10         e ds                  inz  extname(FISP3010ds)
023000080430     d                                          prefix(P : 3)
023100110202       // ·?11ª riga?
023200080430     d ds_11         e ds                  inz  extname(FISP3011ds)
023300080430     d                                          prefix(P : 3)
023400110202       // ·?12ª riga?
023500080430     d ds_12         e ds                  inz  extname(FISP3012ds)
023600080430     d                                          prefix(P : 3)
023700110202       // ·?13ª riga?
023800080430     d ds_13         e ds                  inz  extname(FISP3013ds)
023900080430     d                                          prefix(P : 3)
024000110202       // ·?14ª riga?
024100080430     d ds_14         e ds                  inz  extname(FISP3014ds)
024200080430     d                                          prefix(P : 3)
024300130205     d ds_140        e ds                  inz  extname(FISP30140s)
024400130205     d                                          prefix(P : 3)
024500110202       // ·?15ª riga?
024600080430     d ds_15         e ds                  inz  extname(FISP3015ds)
024700080430     d                                          prefix(P : 3)
024800110202       // ·?16ª riga?
024900080430     d ds_16         e ds                  inz  extname(FISP3016ds)
025000080430     d                                          prefix(P : 3)
025100110202       // ·?17ª riga?
025200080430     d ds_17         e ds                  inz  extname(FISP3017ds)
025300080430     d                                          prefix(P : 3)
025400110202       // ·?18ª riga?
025500080430     d ds_18         e ds                  inz  extname(FISP3018ds)
025600080430     d                                          prefix(P : 3)
025700130205     d ds_180        e ds                  inz  extname(FISP30180s)
025800130205     d                                          prefix(P : 3)
025900110202       // ·?19ª riga?
026000080430     d ds_19         e ds                  inz  extname(FISP3019ds)
026100080430     d                                          prefix(P : 3)
026200110202       // ·?20ª riga?
026300080430     d ds_20         e ds                  inz  extname(FISP3020ds)
026400080430     d                                          prefix(P : 3)
026500110202       // ·?21ª riga?
026600080430     d ds_21         e ds                  inz  extname(FISP3021ds)
026700080430     d                                          prefix(P : 3)
026800110202       // ·?22ª riga?
026900080430     d ds_22         e ds                  inz  extname(FISP3022ds)
027000080430     d                                          prefix(P : 3)
027100110202       // ·?23ª riga?
027200080430     d ds_23         e ds                  inz  extname(FISP3023ds)
027300080430     d                                          prefix(P : 3)
027400110202       // ·?24ª riga?
027500080430     d ds_24         e ds                  inz  extname(FISP3024ds)
027600080430     d                                          prefix(P : 3)
027700110202       // ·?25ª riga?
027800080430     d ds_25         e ds                  inz  extname(FISP3025ds)
027900080430     d                                          prefix(P : 3)
028000110202       // ·?26ª riga (vuota)?
028100110202       // ·?27ª riga?
028200080430     d ds_27         e ds                  inz  extname(FISP3027ds)
028300080430     d                                          prefix(P : 3)
028400110202       // ·?28ª riga?
028500080430     d ds_28         e ds                  inz  extname(FISP3028ds)
028600080430     d                                          prefix(P : 3)
028700110202       // ·?29ª riga?
028800081021     d ds_29         e ds                  inz  extname(FISP3029ds)
028900081021     d                                          prefix(P : 3)
029000110202       // ·?30ª riga?
029100081021     d ds_30         e ds                  inz  extname(FISP3030ds)
029200081021     d                                          prefix(P : 3)
029300110215       // ·?31ª riga?
029400110215     d ds_31         e ds                  inz  extname(FISP3031ds)
029500110215     d                                          prefix(P : 3)
029600110202       // ·?32ª riga?
029700081021     d ds_32         e ds                  inz  extname(FISP3032ds)
029800081021     d                                          prefix(P : 3)
029900110202       // ·?33ª riga?
030000081021     d ds_33         e ds                  inz  extname(FISP3033ds)
030100081021     d                                          prefix(P : 3)
030200110202       // ·?34ª riga?
030300081021     d ds_34         e ds                  inz  extname(FISP3034ds)
030400081021     d                                          prefix(P : 3)
030500110202       // ·?35ª riga (vuota)?
030600110202       // ·?36ª riga (vuota)?
030700110202       // ·?37ª riga e successive?
030800081023     d ds_37         e ds                  inz  extname(FISP3037ds)
030900081021     d                                          prefix(P : 3)
031000110202       // ·?Righe x FedEx?
031100081030     d ds_Fed        e ds                  inz  extname(FISP304Fds)
031200081030     d                                          prefix(P : 3)
031300130411
031400130411       // -?Flag operativi in TITAS30C?
031500130411     d dTASflo       e ds                  inz     qualified
031600120702
031700130214       // -?Dati aggiuntivi in FIILV00F?
031800120702     d dILVlibero    e ds                  inz     qualified
031900101220
032000101220       // -?Parametri per conversione importi in Euro?
032100061107     d YEURCOds      e ds                  inz
032200110202
032300101220       // -?Dati da AZORG00F?
032400070424     d OG143ccm      e ds                  inz     extname(OG143)
032500070424     d                                             prefix(§CM:3)
032600070424     d OG146arr      e ds                  inz     extname(OG146)
032700070424     d                                             prefix(A)
032800070424     d OG146cf£6     e ds                  inz     extname(OG146)
032900070424     d                                             prefix(£6:1)
033000070424     d OG148arr      e ds                  inz     extname(OG148)
033100070424     d                                             prefix(A)
033200070424     d OG148cf£6     e ds                  inz     extname(OG148)
033300070424     d                                             prefix(£6:1)
033400070424     d OG150arr      e ds                  inz     extname(OG150)
033500070424     d                                             prefix(A)
033600110202
033700101220       // -?Dati da FIAR400F?
033800061108     d dsBL4A        e ds                  inz
033900061108     d dsBL4I        e ds                  inz
034000071015     d dsBL4M        e ds                  inz
034100121018     d dTA4Q         e ds                  inz
034200110228
034300110228       // -?Dati da FIDST00F?
034400110228     d dDSTflr       e ds                  inz
034500110202
034600101220       // -?Tabelle?
034700131206     d ds$2          e ds
034800061107     d ds1A          e ds                  inz
034900061107     d ds1P          e ds                  inz
035000070306     d ds2A          e ds                  inz
035100061107     d ds3A          e ds                  inz
035200061107     d ds5E          e ds                  inz
035300061107     d dsCC          e ds
035400061107     d dsCV          e ds                  inz
035500061107     d dsEI          e ds                  inz
035600061107     d dsTB          e ds                  inz
035700061107      *
035800030515     d TIBS02ds      e ds                  inz
035900040607     d   T02mod      e                     inz('C')
036000030805     d dGED          e ds
036100130206     d dILV          e ds
036200061031     d dVPOdeco      e ds                  inz
036300031124     d dAR5gen       e ds                  inz
036400061030     d dAR5bnb       e ds                  inz
036500080724
036600131210       // -?Flag operativi di FNEVB - campo EVBNOT?
036700131206     d dEVB01        e ds                  inz     qualified
036800101220       // -?Flag operativi di FNORM - campo ORMFLO?
036900100517     d dORM01        e ds                  inz
037000101220       // -?Parametri per TRUL21R (calcolo limiti importo c/assegno)?
037100030515     d TRUL21ds      e ds                  inz
037200120702     d   I21tla      e                     inz('L')
037300101220       // -?Parametri per FNLV55R (reperimento terminal arr/par/£6)?
037400070212     d FNLV55ds      e ds                  inz
037500120702     d   D55tpt      e                     inz('6')
037600101220       // -?Parametri per FNLV80R (decodifica consegna richiesta)?
037700051129     d FNLV80ds      e ds                  inz
037800120702     d   ILV80tla    e                     inz('C')
037900101220       // -?Parametri per TNSD99R (calcolo tempi di delivery)?
038000090619     d TNSD99ds      e ds                  inz
038100120702     d   D98tla      e                     inz('C')
038200130227
038300130227       // -?Tab. "MRA" = Bart-Mailing?
038400130227     d dMRAdan       e ds                  inz
038500130227     d   §MRAddes    e                     inz('PDA-Oltre 100 log "01"+
038600130227     d                                             ')
038700130227     d   §MRAdreg    e                     inz('COR')
038800130227     d   §MRAdmitt   e                     inz(c_Mittente)
038900130227     d   §MRAddest   e                     inz('stefano.merighi+
039000130227     d                                          @brt.it;')
039100130227     d   §MRAdidpro  e                     inz('1')
039200130227     d   §MRAdoutqi  e                     inz('EMAILIN')
039300130227
039400130227       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
039500130227     d TRTCM1ds      e ds                  inz
039600130716         //?·§CM1mitt = Indirizzo e-mail del mittente?
039700130227     d   §CM1mitt    e                     inz(c_Mittente)
039800130716         //?·§CM1dst  = Indirizzo e-mail del destinatario?
039900130227     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
040000130716         //?·§CM1tips = Tipo lettera via e-mail:?
040100130716         // ?"LET" = testo allegato in corpo con logo?
040200130716         //         ?(richiede righe libere iniziali per il logo)?
040300130716         // ?"COR" = testo integrato senza logo?
040400130716         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
040500130716         // ?"SP1" = spool allegato (in formato PDF)?
040600130227     d   §CM1tips    e                     inz('COR')
040700130716         //?·§CM1po   = Sede?
040800130227     d   §CM1po      e                     inz(c_Sede)
040900130716         //?·§CM1var  = Oggetto e-mail?
041000130227     d   §CM1var     e                     inz('*OBJM*+
041100130227     d                                     Sped. elaborata oltre 100 +
041200130227     d                                     volte in FIILV - vedi log +
041300130227     d                                     "01"')
041400130716         //?·§CM1sts  = Stato?
041500130227     d   §CM1sts     e                     inz(*off)
041600130716         //?·§CM1idp  = Id processo?
041700130227     d   §CM1idp     e                     inz('2')
041800101220
041900101220       // -?Status ds?
042000030515     d Status         sds
042100120702     d   SDSpgm          *proc
042200120702     d   SDSprm          *parms
042300120702     d   SDSusr              254    263
042400101220
042500101220       // -?Parametro Numero Distinta?
042600101220     d ds_Distinta     ds             9    inz  qualified
042700101220     d   P_Fgs                        3s 0 inz
042800101220     d   P_Ndc                        6s 0 inz
042900080430
043000101207       // -?Parametro Numero Spedizione?
043100110207     d ds_Spedizione   ds            16    inz  qualified
043200080430     d   P_Aas                        4s 0 inz
043300080430     d   P_Lnp                        3s 0 inz
043400080430     d   P_Nrs                        2s 0 inz
043500080430     d   P_Nsp                        7s 0 inz
043600101220
043700080519     d BARcodeDS       ds            17    inz
043800120702     d   BARcdeAAS                    2s 0 inz
043900120702     d   BARcdeLNP                    3s 0 inz
044000120702     d   BARcdeNRS                    2s 0 inz
044100120702     d   BARcdeNSP                    7s 0 inz
044200101220
044300030515     d KEYpar          ds             8    inz
044400120702     d   WTPpar                1      1    inz
044500120702     d   WCDpar                2      3    inz
044600120702     d   Wprogr                4      8    inz('S0001')
044700101220
044800140303     d Elem_CP         ds                  inz
044900120702     d   WCPdcm                       8s 0 inz
045000140303     d   WCPocm                       4s 0 inz
045100120702     d   WCPncl                       5s 0 inz
045200140303     d   WCPfir                            inz  like(EVB2dati)
045300101213
045400101213       //--------------------------------------------------------------
045500101213       //?Definizione variabili globali.                               ?
045600101213       //--------------------------------------------------------------
045700101213
045800101220       // -?Parametri?
045900080430     d Spedizione      s             16
046000101217     d Distinta        s              9
046100101217     d DataOraCrea     s             14
046200080430     d Risultato       s              2
046300080715     d RootFolder      s            100
046400080519     d FileName        s             30
046500080731     d TemplateName    s             20
046600110201     d LastRow         s            120
046700131016     d RtnEsito        s             10i 0                                      // -?NON usato?
046800131016     d RtnOpCode       s             10                                         // -?NON usato?
046900131016     d RtnStatus       s             10i 0                                      // =?SQLcode?
047000080430     d P_Prt           s              1
047100101220
047200130214       // -?Flags booleani?
047300101220     d $Mamma          s               n   inz
047400130215     d $Mamma_FedEx    s               n   inz
047500101220     d $FIAR8          s               n   inz
047600130411     d $§PDF           s               n   inz
047700110204     d $FIILV          s               n   inz
047800101220     d $NoDesPartic    s               n   inz
047900110117     d*// $BolloVirt      s               n   inz(*off)
048000120113     d $No_x_Cons      s               n   inz
048100101220     d $ConsParz       s               n   inz
048200101220     d $CAeur          s               n   inz
048300100517     d $PreavvORM      s               n   inz
048400101220     d $GeoAtt         s               n   inz
048500131206     d $1o_Passaggio   s               n   inz
048600101220
048700101220       // -?Indici di schiera?
048800080523     d XX              s              3  0 inz
048900140304     d YY              s              3  0 inz
049000080523     d WX              s              3  0 inz
049100130313
049200130313       // -?Nome esteso Libreria/File del file TITAS30C?
049300130313     d wLibFileTITAS   s             21a   inz
049400080724
049500101207       // -?Campi di stampa?
049600130227     d P_Fgs2          s                   inz  like(ds_Distinta.P_Fgs)
049700130227     d P_Ndc2          s                   inz  like(ds_Distinta.P_Ndc)
049800101220
049900101220       // -?Campi di comodo alfanumerici?
050000030717     d w_CoOr          s             35    inz
050100081030     d wAWB            s             12    inz
050200030515     d w001a           s              1    inz
050300030515     d w008a           s              8    inz
050400030515     d w015a           s             15    inz
050500100517     d w053a           s             53    inz
050600070424     d wDateA          s              8    inz
050700070717     d wGeoTel         s              1    inz
050800070717     d wGeoCon         s              1    inz
050900080718     d wSysName        s              8    inz
051000130411      *? x TEST ?
051100130411     d $Test           s              3    inz
051200101220
051300101220       // -?Campi di comodo numerici?
051400030515     d w0080           s              8  0 inz
051500030624     d w0112           s             11  2 inz
051600030515     d w0140           s             14  0 inz
051700071105     d wDate           s              8  0 inz
051800090120     d w_Row           s              3  0 inz(c_Row)
051900110309     d wCountRec01     s              5  0 inz
052000101220
052100131008       // -?Campi utilizzati per impostazione dell'output?
052200110117     d*// wBolloVirt      s              4  2 inz
052300030515     d Wcas            s             13  3 inz
052400030515     d Wiva            s             11  2 inz
052500071015     d Wpdc4A          s              4    inz
052600030515     d Data_EUR        s               d   datfmt(*eur)   inz(*loval)
052700090619     d Data_DMY        s               d   datfmt(*dmy)   inz(*loval)
052800101220
052900101220       // -?Campi utilizzati per il salvataggio momentaneo di dati?
053000040311     d SAVlna          s                   like(DB0lna)   inz(*loval)
053100121018     d wB0cfi          s                   like(DB0cpi)   inz
053200121018     d wB0piv          s                   like(DB0cpi)   inz
053300120702     d wB0nrs          s                   like(ARBnrs)   inz
053400070514     d wB0ntc          s                   like(ARBntc)   inz
053500080523     d wB0dcm          s                   like(ARBdcm)   inz
053600070514     d wB0dcp          s                   like(ARBdcp)   inz
053700081120     d wB0tmc          s                   like(ARBtmc)   inz
053800130205     d wB0fiv          s                   like(AR6fiv)   inz
053900080506     d Wnsc            s                   like(ARTnsc)   inz
054000080506     d Wtrc            s                   like(AR4trc)   inz
054100030515     d Wctr            s                   like(DB0ctr)   inz
054200031124     d Wdsp            s                   like(DB0dft)   inz
054300090619     d WddtCLN         s                   like(DB0drt)   inz
054400090619     d WdspCLN         s              6  0                inz
054500030515     d Wsvx            s                   like(AR6sv1)   inz
054600030515     d Wvax            s                   like(AR6va1)   inz
054700110203     d wCod_Log        s                   like(ILVcol)   inz
054800090619
054900140303       //--------------------------------------------------------------
055000140303       //?Definizione procedure usate.                                 ?
055100090619       //--------------------------------------------------------------
055200100322
055300100322       // -?Procedure name: getCurrentSystemName?
055400100322       //  ?Purpose:        Reperisce l'attributo di rete current System Name?
055500100322       //  ?Returns:        Current System Name?
055600130716       //  ?(vedi "QWCRNETA")?
055700100322     d getCurrentSystemName...
055800100322     d                 pr             8a
055900100322
056000100322       //--------------------------------------------------------------
056100140303       //?Definizione procedure esterne.                               ?
056200100322       //--------------------------------------------------------------
056300100322
056400110201       // -?Calcolo check digit del barcode?
056500110201     d TRUL28ds      e ds                  inz
056600110201     d  I28tla       e                     inz('L')
056700110201     d  I28mod       e                     inz('BAR')
056800110201     d TrUl28r1        pr                  extpgm('TRUL28R1')
056900110201     d  trul28ds                           likeds(TRUL28ds)
057000110201
057100100322       // -?Calcolo tempi di delivery?
057200100322      /copy gaitrasrc/srcProtoPR,TNSD99R
057300130206
057400130206       // -?Ricerca/Controllo tabelle?
057500130206      /copy gaitrasrc/srcProtoPR,TIBS02R
057600100322
057700110201       // -?Parametri per API "QWCRNETA"?
057800110201      /copy QSYSINC/QRPGLESRC,QWCRNETA
057900110201     d qWcrNetA_dataReturned...
058000110201     d                 ds                  qualified inz
058100110201     d  qWcRDR                             likeds(qWCRDR)
058200110201     d  qWcOAT                       10i 0 dim(00001)
058300110201     d  qWcARRAY                     24
058400110201     d qWCRNETA_networkAttributeInformationTable...
058500110201     d                 ds                  qualified inz
058600110201     d  qWcRAT                             likeds(qWCRAT)
058700110201     d  qWcDATA                       8
058800100322       // -?Reperimento attributo di rete current System Name?
058900100322       //  ?(vedi "getCurrentSystemName")?
059000100322      /copy gaitrasrc/srcProtoPR,QWCRNETA
059100130227
059200130227       // -?Parametri API QCAPCMD (Process Commands)?
059300130227     d Qcmd            s           2048    inz  varying
059400130227      /copy qSysInc/qRpgleSrc,QCAPCMD
059500130227       // -?API QCAPCMD (Process Commands)?
059600130227      /copy gaitrasrc/srcProtoPR,QCAPCMD
059700130227
059800130227       // -?Parametri gestione errori API.?
059900130227      /copy qsysinc/qRpgleSrc,QUSEC
060000080710
060100080710       //--------------------------------------------------------------
060200140303       //?Definizione key-list.                                        ?
060300080710       //--------------------------------------------------------------
060400130313
060500130422       // -?File FIIRD11L?
060600130422     d k01fiird11    e ds                  extname(FIIRD11L : *key)
060700130313     d                                     prefix(k_)
060800130313     d                                     inz
060900130313
061000130313       // -?File TITAS30C?
061100130313     d k05titas30    e ds                  extname(TITAS30C : *key)
061200130313     d                                     prefix(k_)
061300130313     d                                     inz
061400131206
061500131206       // -?File FNEVB04L?
061600131206     d keyFNEVB04    e ds                  extname(FNEVB04L : *key)
061700131206     d                                     prefix(k_)
061800131206     d                                     inz
061900140303
062000140303       // -?File FNEVB22L?
062100140303     d keyFNEVB22    e ds                  extname(FNEVB22L : *key)
062200140303     d                                     prefix(k_)
062300140303     d                                     inz
062400080710
062500101207       // -?File FIDST01L?
062600110228     d k03fidst01    e ds                  extname(FIDST01L : *key)
062700080710     d                                     prefix(k_)
062800080710     d                                     inz
062900090216
063000110202       // -?File FIILV01L?
063100110202     d k06fiilv01    e ds                  extname(FIILV01L : *key)
063200110207     d                                     prefix(k_)
063300090216     d                                     inz
063400030515
063500101220       //==============================================================
063600101220       //?RIEPILOGO INDICATORI                                         ?
063700101220       //--------------------------------------------------------------
063800101220       // 31    - di comodo
063900101220       // 35    - Porto Franco: segnala
064000101220       //         "PER RICEVUTA DELLA MERCE SENZA RISERVE"
064100101220       //?36   ?- Richiesta FIRMA per QUIETANZA in 2ª copia
064200101220       //         e indica "TOTALE DA ESIGERE AL RITIRO"
064300120524       // 70    - Trattasi di BOLLA BRT
064400101220       // 71    - Trattasi di BOLLA DPD
064500101220       // 72    - Trattasi di BOLLA FedEx
064600101220       // 73    - Trattasi di BOLLA EuroExpress
064700101220       //==============================================================
064800030515
064900101213       //--------------------------------------------------------------
065000101213       //?M A I N - L I N E                                            ?
065100101213       //--------------------------------------------------------------
065200101213
065300030515     c     *Entry        plist
065400080430     c                   parm                    Spedizione
065500101217     c                   parm                    Distinta
065600101217     c                   parm                    DataOraCrea
065700080430     c                   parm                    Risultato
065800080715     c                   parm                    RootFolder
065900080519     c                   parm                    FileName
066000080731     c                   parm                    TemplateName
066100110201     c                   parm                    LastRow
066200080430     c                   parm                    RtnEsito
066300080430     c                   parm                    RtnOpCode
066400080430     c                   parm                    RtnStatus
066500080430     c                   parm                    P_Prt
066600080430      *
066700110519      *?P_Prt:?non passato se richiamato da server, serve solo se?
066800110519      *       ?chiamato da menù AS400 x test?
066900101221      *   Valori: qualsiasi = richiamato da pgm. per test (stampa)
067000080430      *
067100070123      *
067200130924      * Operazioni iniziali
067300131016       //?SE rilevati errori nei parametri di input =>?
067400131016       //?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
067500070123     c                   exsr      sr_Start
067600030910      *
067700130924      * Pulizia dei campi di output e/o di lavoro
067800130924     c                   exsr      sr_ClrFLD
067900030515      *
068000130924      * Caricamento dei dati per l'immagine della LdV
068100131016       //?SE rileva errori nella distinta o nella spedizione =>?
068200131016       //?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
068300110208     c                   exsr      sr_CarARB
068400030515      *
068500130924      * Controllo del tipo LDV di cui generare l'immagine
068600110208     c                   exsr      sr_TipoLDV
068700030515      *
068800080430      * Impostazione dei campi di output con quelli della DS
068900110208     c                   exsr      sr_PrepPRT
069000030515      *
069100080430      * Decodifica dei dati della LDV
069200110208     c                   exsr      sr_DecodLDV
069300080806      *
069400080806      * Impostazione dati per immagine LdV
069500131016     c                   exsr      sr_DatiOUT
069600030515      *
069700030515      * Chiusura pgm
069800080430     c                   exsr      sr_End
069900030515      *
070000030515      *---------------------------------------------------------------*
070100050217      *?OPERAZIONI INIZIALI                                          ?*
070200030515      *---------------------------------------------------------------*
070300030515     c     *InzSR        BEGSR
070400070424      *
070500070424      * Reperisco la data del giorno
070600070424     c                   time                    w0140
070700070424     c                   move      w0140         w0080
070800070424     c     *eur          movel     w0080         Data_EUR
070900070424     c     *iso          movel     Data_EUR      w0080
071000070424     c                   move      w0080         wDateA
071100071105     c                   eval      wDate  = w0080
071200030515      *
071300030515      * Imposto i campi chiave "fissi"
071400040223     c                   eval      TBLkut = 1
071500100901     c                   eval      ARStrc = 'C'
071600030515      *
071700040617      * Impostazione dati in KPJBA
071800070308     c                   eval      KNMUS  = SDSusr
071900030515      *
072000030515      * Reperisco la moneta di conto
072100030515     c                   clear                   dGED
072200040607     c                   clear                   TIBS02ds
072300040607     c                   eval      T02mod = 'C'
072400040607     c                   eval      T02cod = 'GED'
072500040607     c                   eval      T02ke1 = '1         '
072600030515     c                   call      'TIBS02R'
072700030515     c                   parm                    KPJBA
072800040607     c                   parm                    TIBS02ds
072900030515if  1c                   if        T02err = *blanks
073000030515     c                   movel     T02uni        dGED
073100030515e   1c                   endif
073200061031      *
073300131008      * Reperisco data di decorrenza indicaz. "Per ricevuta di ...
073400131008      *                                        senza riserve"
073500131008      *              e di decorrenza indicaz. "Consegna al mattino"
073600061031     c                   clear                   dVPOdeco
073700061031     c                   clear                   TIBS02ds
073800061031     c                   eval      T02mod = 'C'
073900071112     c                   eval      T02cod = 'VPO'
074000061031     c                   eval      T02ke1 = 'DECO      '
074100061031     c                   call      'TIBS02R'
074200061031     c                   parm                    KPJBA
074300061031     c                   parm                    TIBS02ds
074400061031if  1c                   if        T02err = *blanks
074500061031     c                   movel     T02uni        dVPOdeco
074600061031e   1c                   endif
074700030515      *
074800030515     c                   ENDSR
074900070123      *
075000101220      /free
075100101220
075200101220       //--------------------------------------------------------------
075300101220       //?ALTRE OPERAZIONI INIZIALI (NON PIÙ IN *INZSR)                ?
075400101220       //--------------------------------------------------------------
075500101220       BEGSR  sr_Start;
075600101220
075700101220         exec sql  set option  dynusrprf = *owner,
075800101220                               closqlcsr = *endmod;
075900101220
076000101221         *inLR = *on;
076100080509
076200110517         // -?Impostazione iniziale dei dati di ritorno?
076300080509         Risultato = 'OK';
076400110202         clear  RootFolder;
076500110202         clear  FileName;
076600120524         //TemplateName = 'ldvbase00 ';   //?Standard - BARTOLINI senza box aggiuntivo?
076700120524         //TemplateName = 'ldvbase10 ';   //?Standard - BARTOLINI-BRT senza box aggiuntivo?
076800130924         TemplateName = 'ldvbase20 ';     //?Standard - BRT senza box aggiuntivo?
076900110202         clear  LastRow;
077000110202         clear  RtnEsito;
077100110202         clear  RtnOpCode;
077200110202         clear  RtnStatus;
077300110207
077400110207         // -?Apertura archivio di output - comunque utilizzato?
077500110207         if  NOT  %open(FIILV01L);
077600110207           open(e)  FIILV01L;
077700110207           if  %error;
077800110207             exsr  sr_ErrOpenFile;
077900110207           endif;
078000110207         endif;
078100110207
078200110202         clear  k06fiilv01;
078300110203         clear  FIILV000;
078400080509
078500110203         // »? STEP 0 ?«
078600131016         // -?Controllo parametri ricevuti.  SE rileva errori =>?
078700131016         //  ?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
078800130716         exsr  sr_STEP_0_CtrlParms;
078900110203
079000080430
079100101213         // -?Impostazione spedizione ricevuta come parametro?
079200080430         ds_Spedizione = Spedizione;
079300110126         // -?Impostazione distinta ricevuta come parametro?
079400110126         ds_Distinta = Distinta;
079500080430
079600101213         // -?Impostazione iniziale dei dati nella ds FNLSB5DS?
079700130924         //clear FNLSB5ds;            ?(già così)?
079800080430         DB0tbo = 'A';
079900110202         DB0aas = ds_Spedizione.P_Aas;
080000110202         DB0lnp = ds_Spedizione.P_Lnp;
080100110202         DB0nrs = ds_Spedizione.P_Nrs;
080200110202         DB0nsp = ds_Spedizione.P_Nsp;
080300080519
080400101213         // -?Impostazione campo di output "identificativo spedizione"?
080500080519         clear  BarCodeDS;
080600080519         BARcdeAAS = %int( %subst( %editc( DB0aas : 'X' ) : 3 : 2) );
080700080519         BARcdeLNP = DB0lnp;
080800080519         BARcdeNRS = DB0nrs;
080900080519         BARcdeNSP = DB0nsp;
081000080519         FileName  = BarCodeDS;
081100130313
081200130716         // -?Impostazione chiave di accesso al file FIIRD11L?
081300130313         k_IRDfgsndc = Distinta;
081400130313
081500130313         // -?Impostazione chiave di accesso al file TITAS30C?
081600130313         clear  k05titas30;
081700130313         k_TASaas = DB0aas;
081800130313         k_TASlnp = DB0lnp;
081900130313         k_TASnrs = DB0nrs;
082000130313         k_TASnsp = DB0nsp;
082100131206
082200131206         // -?Impostazione chiave di accesso al file FNEVB04L?
082300131206         clear  keyFNEVB04;
082400131206         k_EVBaas = DB0aas;
082500131206         k_EVBlnp = DB0lnp;
082600131206         k_EVBnrs = DB0nrs;
082700131206         k_EVBnsp = DB0nsp;
082800131210         //k_EVBcev = 'MIC';          ?(leggere TUTTI i rec.)?
082900140303
083000140303         // -?Impostazione chiave di accesso al file FNEVB22L?
083100140303         clear  keyFNEVB22;
083200140303         k_EVB2aas = DB0aas;
083300140303         k_EVB2lnp = DB0lnp;
083400140303         k_EVB2nrs = DB0nrs;
083500140303         k_EVB2nsp = DB0nsp;
083600140303         //k_EVB2dtv = .....;
083700140303         //k_EVB2orv = .....;
083800140303         //k_EVB2cev = 'P  ';
083900110204
084000110204         // -?Impostazione chiave di accesso al file FIILV01L?
084100110204         k_ILVaas = DB0aas;
084200110204         k_ILVlnp = DB0lnp;
084300110204         k_ILVnrs = DB0nrs;
084400110204         k_ILVnsp = DB0nsp;
084500110225         k_ILVfgs = ds_Distinta.p_Fgs;
084600110225         k_ILVndc = ds_Distinta.p_Ndc;
084700120702         clear  dILVlibero;
084800130411
084900130411
085000130411         IF  '1' <> '1';
085100130411
085200130924           // -? CHIODI MOMENTANEI PER TEST                                 ?
085300130924           //  ? da attivare per ricavare immagini di esempio forzando sped ?
085400130924           clear  $Test;
085500130411           Select;
085600130411             // -?Spedizione FedEx?
085700130411             When  Spedizione = '2011050001843581' and Distinta = '145040335';
085800130411               $Test = 'FED';
085900130411             // -?Spedizione DPD?
086000130411             When  Spedizione = '2011190000579210' and Distinta = '145040339';
086100130411               $Test = 'DPD';
086200130411             // -?Spedizione con Consegna Parziale Precedente?
086300130411             When  Spedizione = '2011001000816011' and Distinta = '145040361';
086400130411               $Test = 'C_P';
086500130411             // -?Spedizione Normale?
086600130411             When  Spedizione = '2011163000042281' and Distinta = '145040333';
086700130411               $Test = 'NOR';
086800130411             // -?Altra spedizione: NON ELABORABILE?
086900130411             Other;
087000130411               Risultato = 'NO';
087100130924               //RtnOpCode = 'NO_IN_TEST';
087200130924               //RtnEsito  = -1;
087300130924               // -?Uscita dalla s.p.?
087400130411               exsr  sr_End;
087500130411           EndSl;
087600130411
087700130411         ENDIF;
087800130411
087900080430
088000110208         // -?Apertura archivi di input - comunque utilizzati?
088100110207         if  NOT  %open(TABEL00F);
088200110207           open(e)  TABEL00F;
088300110207           if  %error;
088400110207             exsr  sr_ErrOpenFile;
088500110207           endif;
088600110207         endif;
088700110207
088800110207         if  NOT  %open(AZORG01L);
088900110207           open(e)  AZORG01L;
089000110207           if  %error;
089100110207             exsr  sr_ErrOpenFile;
089200110207           endif;
089300110207         endif;
089400110207
089500110207         if  NOT  %open(FNARB01L);
089600110207           open(e)  FNARB01L;
089700110207           if  %error;
089800110207             exsr  sr_ErrOpenFile;
089900110207           endif;
090000110207         endif;
090100110207         if  NOT  %open(FIAR601L);
090200110207           open(e)  FIAR601L;
090300110207           if  %error;
090400110207             exsr  sr_ErrOpenFile;
090500110207           endif;
090600110207         endif;
090700110207         if  NOT  %open(FIAR701L);
090800110207           open(e)  FIAR701L;
090900110207           if  %error;
091000110207             exsr  sr_ErrOpenFile;
091100110207           endif;
091200110207         endif;
091300110207         if  NOT  %open(FIAR901L);
091400110207           open(e)  FIAR901L;
091500110207           if  %error;
091600110207             exsr  sr_ErrOpenFile;
091700110207           endif;
091800110207         endif;
091900110207         if  NOT  %open(FIDST01L);
092000110207           open(e)  FIDST01L;
092100110207           if  %error;
092200110207             exsr  sr_ErrOpenFile;
092300110207           endif;
092400110207         endif;
092500101213
092600101213         if  NOT  %open(FIAR401L);
092700101213           open(e)  FIAR401L;
092800101213           if  %error;
092900101213             exsr  sr_ErrOpenFile;
093000101213           endif;
093100101213         endif;
093200101213         if  NOT  %open(FIAR501L);
093300101213           open(e)  FIAR501L;
093400101213           if  %error;
093500101213             exsr  sr_ErrOpenFile;
093600130411           endif;
093700130411         endif;
093800101213
093900101220       ENDSR;
094000101220
094100101213       //--------------------------------------------------------------
094200101213       //?Restituzione errore in fase di apertura di un file           ?
094300101213       //--------------------------------------------------------------
094400101213       BEGSR  sr_ErrOpenFile;
094500101213
094600101213         Risultato = 'NO';
094700130924         //RtnOpCode = 'ERR_OPEN  ';
094800101213         //RtnEsito  = -1;
094900101213
095000130924         // -?Uscita dalla s.p.?
095100101213         exsr  sr_End;
095200101213
095300101213       ENDSR;
095400130716
095500130716       //--------------------------------------------------------------
095600130716       //?»  STEP 0  «                                                 ?
095700130716       //?Controllo dei parametri ricevuti, reperimento nome del       ?
095800130716       //?sistema e dei percorsi per le immagini in base ad esso.      ?
095900130716       //--------------------------------------------------------------
096000130716       BEGSR  sr_STEP_0_CtrlParms;
096100130716
096200130716         //   _________________________________________________________
096300130716         // -? Controllo lunghezza e numericità dei parametri ricevuti ?
096400130716         //   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
096500130716         SELECT;
096600130716
096700130716           // -?NUMERO SPEDIZIONE?
096800130716           WHEN  ( %len( %trimr( Spedizione ) ) )  <> 16     or
096900130716                 ( %check (c_Digits : Spedizione) ) > *zero  or
097000130716                                      Spedizione   <= *zero;
097100130716             Risultato = 'XX';
097200130924             //RtnOpCode = 'ERR_NUMSPE';
097300130716             //RtnEsito  = -1;
097400130716             // -?Registrazione log in FIILV?
097500130716             wCod_Log = '21';
097600130716             exsr  sr_WrtFIILV;
097700130716             // -?Uscita dalla s.p.?
097800130716             exsr  sr_End;
097900130716
098000130716           // -?NUMERO DISTINTA?
098100130716           WHEN  ( %len( %trimr( Distinta ) ) )  <> 9      or
098200130716                 ( %check (c_Digits : Distinta) ) > *zero  or
098300130716                                      Distinta   <= *zero;
098400130716             Risultato = 'XX';
098500130924             //RtnOpCode = 'ERR_DISTIN';
098600130716             //RtnEsito  = -1;
098700130716             // -?Registrazione log in FIILV?
098800130716             wCod_Log = '21';
098900130716             exsr  sr_WrtFIILV;
099000130716             // -?Uscita dalla s.p.?
099100130716             exsr  sr_End;
099200130716         ENDSL;
099300130716
099400130716         // ________________________________________________________
099500130716         //? Reperimento nome del sistema per impostazione percorso ?
099600130716         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
099700130716         wSysName = getCurrentSystemName();
099800130716         if  wSysName = *blank;
099900130716           Risultato  = 'NO';
100000130924           //RtnOpCode  = 'SYS_ERR   ';
100100130924           //RtnEsito   = -1;
100200130924           // -?Uscita dalla s.p.?
100300130716           exsr  sr_End;
100400130716         endif;
100500130716
100600130716         // ______________________________________________________
100700130716         //? Reperimento dei percorsi in base al nome del sistema ?
100800130716         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
100900130716         clear  dILV;
101000130716         reset  TIBS02ds;
101100130716         T02cod = 'ILV';
101200130716         T02ke1 = %subst( wSysName : 1 : 6 );
101300130716         T02ke2 = 'LINEA';
101400130716         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
101500130716         if  T02err = *blank;
101600130716           dILV = T02uni;
101700130716         endif;
101800130716
101900130716       ENDSR;
102000110208
102100110208       //--------------------------------------------------------------
102200130924       //?Pulizia dei campi di output e/o di lavoro                    ?
102300110208       //--------------------------------------------------------------
102400130924       BEGSR  sr_ClrFLD;
102500080430
102600081021         clear ds_01;
102700101213       //clear ds_02;                      ?(riga vuota)?
102800080430         clear ds_03;
102900080430         clear ds_04;
103000080430         clear ds_05;
103100101213       //clear ds_06;                      ?(riga vuota)?
103200080430         clear ds_07;
103300101213       //clear ds_08;                      ?(riga vuota)?
103400080430         clear ds_09;
103500080430         clear ds_10;
103600080430         clear ds_11;
103700080430         clear ds_12;
103800080430         clear ds_13;
103900080430         clear ds_14;
104000130205         clear ds_140;
104100080430         clear ds_15;
104200080430         clear ds_16;
104300080430         clear ds_17;
104400080430         clear ds_18;
104500130205         clear ds_180;
104600080430         clear ds_19;
104700080430         clear ds_20;
104800080430         clear ds_21;
104900080430         clear ds_22;
105000080430         clear ds_23;
105100080430         clear ds_24;
105200080430         clear ds_25;
105300101213       //clear ds_26;                      ?(vuoto)?
105400080430         clear ds_27;
105500080430         clear ds_28;
105600081021         clear ds_29;
105700081021         clear ds_30;
105800110215         clear ds_31;
105900081021         clear ds_32;
106000081021         clear ds_33;
106100081021         clear ds_34;
106200081023         clear ds_37;
106300081030         clear ds_Fed;
106400080430
106500080430      /end-free
106600030515      *
106700060524     c                   clear                   dsBL4A
106800061030     c                   clear                   dsBL4I
106900110204     c                   clear                   dsBL4M
107000100517     c                   clear                   dOrm01
107100041207      *
107200070514     c                   reset                   $Mamma
107300130215     c                   reset                   $Mamma_FedEx
107400060921     c                   reset                   $FIAR8
107500130411     c                   reset                   $§PDF
107600110204     c                   reset                   $FIILV
107700030710     c                   reset                   $NoDesPartic
107800110117     c*//                reset                   $BolloVirt
107900040707     c                   reset                   $No_x_Cons
108000070319     c                   reset                   $ConsParz
108100030515     c                   reset                   $CAeur
108200100901     c                   reset                   $PreavvORM
108300101220     c                   reset                   $GeoAtt
108400070717     c                   clear                   wGeoTel
108500070717     c                   clear                   wGeoCon
108600070717     c                   clear                   Wdsp
108700090619     c                   clear                   WddtCLN
108800090619     c                   clear                   WdspCLN
108900030717     c                   clear                   W_CoOr
109000110117     c*//                clear                   wBolloVirt
109100030515     c                   clear                   Wcas
109200030515     c                   clear                   Wiva
109300071015     c                   clear                   Wpdc4a
109400121018     c                   clear                   wB0cfi
109500121018     c                   clear                   wB0piv
109600031015     c                   clear                   wB0nrs
109700070514     c                   clear                   wB0ntc
109800080523     c                   clear                   wB0dcm
109900070514     c                   clear                   wB0dcp
110000081120     c                   clear                   wB0tmc
110100130205     c                   clear                   wB0fiv
110200030515      *
110300030515     c                   clear                   $NP
110400070319     c                   reset                   $CP
110500070319     c                   reset                   $CPdcm
110600030515      *
110700030515     c                   movea     *zeros        *in(01)
110800030515      *
110900030515     c                   ENDSR
111000030515      *
111100030515      *---------------------------------------------------------------*
111200131008      *?Determino il modulo LDV da usare per l'immagine              ?*
111300131008      *?(se DPD, FedEx o BRT - con consegne parziali precedenti o    ?*
111400131008      *? promrmoria per l'autotrasportatore).                        ?*
111500030515      *---------------------------------------------------------------*
111600110208     c     sr_TipoLDV    BEGSR
111700070215      *
111800070212      * Impostazione della data spedizione
111900110208      * (se non già impostata in subr. "sr_CarARB")
112000070212if  1c                   if        Wdsp   =  *zeros
112100070212if  2c                   if        DB0mgs <> *zeros
112200070212     c                   eval      Wdsp   =  (DB0aas * 10000) + DB0mgs
112300070212x   2c                   else
112400070212      *   prendo la data del giorno se non c'è la data spedizione
112500070212     c                   time                    w0140
112600070212     c                   move      w0140         w0080
112700070212     c     *eur          movel     w0080         Data_EUR
112800070212     c     *iso          movel     Data_EUR      Wdsp
112900070212e   2c                   endif
113000070212e   1c                   endif
113100040607      *
113200070308      * Dati della filiale del mittente
113300040607     c                   eval      WX     =  DB0ccm / 10000
113400040607if  1c                   if        WX     <> ORGfil
113500040607     c     WX            chain     AZORG
113600040607e   1c                   endif
113700040607if  1c                   if        %found(AZORG01L)
113800040607     c                   movel     ORGde3        OG143ccm
113900040607x   1c                   else
114000040607     c                   clear                   OG143ccm
114100040607e   1c                   endif
114200040311      *
114300040311      * Dati della linea di arrivo
114400131203      *?(SONO GIÀ STATI REPERITI NELLA SUBR. "SR_DECLNA",?
114500131203      *? ESEGUITA DALLA SUBR. "SR_CARARB")?
114600070212      *
114700070212      * Dati della capofila della linea di arrivo
114800131205      * - per verificare TELLUS
114900070717      *
115000070717      *** Imposta flag attivazione GEO x LNA
115100070717     c                   eval      wGeoCon = A§OGtlls
115200070717     c                   eval      wGeoTel = A§OGgeot
115300070717if  1c                   if             A§OGdda  > *zeros
115400070717     c                             and  A§OGdda  < C_DateMax_A
115500070717     c                             and  A§OGdda <= wDateA
115600101220     c                   eval      $GeoAtt = *on
115700070717e   1c                   endif
115800070717      *
115900110203if  1c                   IF             wGeoCon <> 'S'
116000070717     c                              or  wGeoTel <> 'S'
116100110203     c                              or  $GeoAtt <> *on
116200070212      * - Reperimento capofila (£6)
116300070212if  2c                   if        DB0lna <> D55lin
116400070212     c                   reset                   FNLV55ds
116500130924     c***?già così:?     eval      D55tpt =  '6'
116600070212     c                   eval      D55lin =  DB0lna
116700070212     c                   eval      D55drf =  Wdsp
116800070212     c                   call      'FNLV55R'
116900070212     c                   parm                    FNLV55ds
117000070212if  3c                   if        D55err <> *blanks
117100070212     c                   eval      D55tfa =  DB0lna
117200070212e   3c                   endif
117300070212e   2c                   endif
117400070321      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
117500070306      *   (flag Tellus del capofila e flag gestione telefonate GEO)
117600070212if  2c                   if        D55tfa <> ORGfil
117700070212     c     D55tfa        chain     AZORG
117800071109if  3c                   if        %found(AZORG01L)
117900070717     c                   movel     ORGde6        OG146cf£6
118000070717     c                   movel     ORGde8        OG148cf£6
118100071109x   3c                   else
118200071109     c                   clear                   OG146cf£6
118300071109     c                   clear                   OG148cf£6
118400071109e   3c                   endif
118500071109e   2c                   endif
118600070717      *** Imposta flag attivazione GEO x capofila £6
118700070717     c                   eval      wGeoCon = £6OGtlls
118800070717     c                   eval      wGeoTel = £6OGgeot
118900071109if  2c                   if             £6OGdda  > *zeros
119000070717     c                             and  £6OGdda  < C_DateMax_A
119100070717     c                             and  £6OGdda <= wDateA
119200101220     c                   eval      $GeoAtt = *on
119300071109e   2c                   endif
119400070717      *
119500070212e   1c                   ENDIF
119600040607      *
119700130411      * Reperimento dati da FIAR5 / "GEN"
119800130411      *   (per verificare se esistono Packing-List e/o PDF allegati
119900130411      *    - oltre che per reperire Nominativo e Telefono)
120000130411     c                   exsr      RepAR5gen
120100130411      *
120200130411      * Verifica se LDV con DDT e/o PDF del mittente
120300130411     c                   exsr      CercaDDToPDF
120400131206      *
120500131206      * Controllo se già effettuato il 1° passaggio c/o destinatario
120600131206     c                   exsr      sr_Check1pass
120700070319      *
120800070319      * Verifica se richiesta spedizione già parzialmente consegnata
120900070319      *   (da stampare nel box "Note per l'autotrasportatore")
121000070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
121100070717      * completamente
121200110203if  1c                   if             wB0dcp   >  *zeros
121300101220     c                             and  $GeoAtt  =  *on
121400070413     c                   eval      $ConsParz     =  *on
121500070319e   1c                   endif
121600030515      *
121700040607      * Tipo LDV (per richiesta firma per ricevuta della merce senza
121800040607      *   riserve)
121900120524      *   (70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)
122000040223     c                   movea     '1000'        *in(70)
122100040311      *
122200030515sel 1c                   SELECT
122300071213      *
122400131203      * LDV DPD (dalla fil. del mittente, NON della LNP)
122500071213w   1c                   WHEN      §CMntw = 'DPD'
122600071213     c                   movea     '0100'        *in(70)
122700071213      *
122800071213      * LDV con mittente FedEx
122900150430w   1c                   WHEN      DB0ccm = c_CliFedEx   or
123000150430     c                             DB0ccm = c_CliFedEx2
123100071213     c                   movea     '0010'        *in(70)
123200030515      *
123300030515e   1c                   ENDSL
123400040707      *
123500040707      * Controllo se devo stampare "Non utilizzare per la consegna"
123600040707      * nella copia per il vettore,?senza?copia per il destinatario:
123700130924     c***?(già fatto:)?  reset                   $No_x_Cons
123800071203      * - LdV recapito c/assegni:
123900100322      *   stampata in arrivo e con
124000120113if  1c                   if             DB0lnp =  102
124100070306     c                             and  DB0cbo =  'FW'
124200071203     c                             and  DB0nft =  *zeros
124300071203      *   - fil. arrivo con GEO attivato
124400071203     c                             and (DB0fl1    =  'A'
124500071203     c                              or  wGeoCon   =  'S'
124600071203      *   - fil. arrivo abilitata al picking
124700120113     c                              or  A§OGpck   =  'S')
124800070306     c                   eval      $No_x_Cons  =  *on
124900120113e   1c                   endif
125000030515      *
125100030515     c                   ENDSR
125200040607      *
125300040607      *---------------------------------------------------------------*
125400130411      *?Verifica esistenza dati del DDT allegato da stampare  o      ?*
125500130411      *?               di uno o più PDF allegati da stampare         ?*
125600040607      *---------------------------------------------------------------*
125700130411     c     CercaDDToPDF  BEGSR
125800040607      *
125900130924     c***?(già fatto:)?  reset                   $FIAR8
126000130924     c***?(già fatto:)?  reset                   $§PDF
126100040607      *
126200130924     c***?(già fatto:)?  exsr      RepAR5gen
126300130411      *
126400130411      * -?Verifica esistenza PDF allegato?
126500130411     c                   eval      $§PDF  = (§AR5als = 'S')
126600130411      *
126700130411      * -?Verifica esistenza packing-list?
126800130411     c                   eval      $FIAR8 = (§AR5alx = 'S')
126900040607      *
127000080505     c                   ENDSR
127100131206      *
127200131206      /free
127300131206
127400131206       //--------------------------------------------------------------
127500131206       //?Verifica se già effettuato il 1° passaggio.                  ?
127600131206       //--------------------------------------------------------------
127700131206       BEGSR  sr_Check1pass;
127800131206
127900131206         //reset  $1o_Passaggio       ?(già fatto)?
128000131206
128100131206         // -?Apertura archivio?
128200131206         If  NOT  %open(FNEVB04L);
128300131206           open(e)  FNEVB04L;
128400131206           if  %error;
128500131206             exsr  sr_ErrOpenFile;
128600131206           endif;
128700131206         EndIf;
128800131206
128900131206         // -?Ciclo di lettura per ricercare l'eventuale 1° passaggio?
129000131210         setgt   %kds(keyFNEVB04 : 4)  FNEVB000;
129100131210         readPe  %kds(keyFNEVB04 : 4)  FNEVB000;
129200131206
129300131206         DoW  Not %eof(FNEVB04L)  and  Not $1o_Passaggio;
129400131206
129500131206           dEVB01 = EVBnot;
129600131206           $1o_Passaggio = (dEVB01.§NOTprg = 'P');
129700131206
129800131206           if  Not $1o_Passaggio;
129900131210             readPe  %kds(keyFNEVB04 : 4)  FNEVB000;
130000131206           endif;
130100131206
130200131206         EndDo;
130300131206
130400131206       ENDSR;
130500131206
130600131206      /end-free
130700030515      *
130800030515      *---------------------------------------------------------------*
130900050217      *?Imposta i campi di stampa con quelli della DS                ?*
131000030515      *---------------------------------------------------------------*
131100110208     c     sr_PrepPRT    BEGSR
131200040223      *
131300040223      * Reperimento dati da FIAR5 / "GEN"
131400130924     c***?(già fatto:)?  exsr      RepAR5gen
131500070212     *** *
131600070212     *** * Impostazione Data spedizione
131700110208     *** * (già fatto nella subr. "sr_TipoLdV")
131800030515      *
131900030515      * - Intestazione moduli
132000030515      *
132100030515      * - Sigla operatore video
132200030515     c                   eval      Psop   =  DB0sop
132300081120      /free
132400081120         //
132500130411         // -?Flag "allega documento di trasporto"?
132600081120         select;
132700081120
132800130411           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
132900081120           when  DB0bam =  'S'   and   wB0tmc <> *blank;
133000081120             evalr Pddt =  'DDT sul collo';
133100081120
133200130411           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
133300081120           when  DB0bam =  'S';
133400081120             evalr Pddt =  'DDT';
133500081120
133600130716           // -?L'indicazione di packing-list è stata spostata in PPKL?
133700130716           //// -?Esiste packing-list abbinata?
133800130411           //when  $FIAR8 =  *on   and   $§PDF =  *off;
133900131203           //  evalr Pddt =  'PACKING-LIST';
134000130716           //// -?LdV con PDF allegato?
134100130411           //when  $FIAR8 =  *off  and   $§PDF =  *on;
134200130411           //  evalr Pddt =  'PACKING-LIST PDF';
134300130716           //// -?LdV con packing-list abbinata & PDF allegato?
134400130411           //when  $FIAR8 =  *on   and   $§PDF =  *on;
134500130411           //  evalr Pddt =  'PACKING-LIST PDF';
134600130411
134700130411           // -?LdV senza PDF allegato & senza packing-list abbinata?
134800081120           other;
134900081120             clear Pddt;
135000081120
135100081120         endsl;
135200081120
135300081120      /end-free
135400030515      *
135500030515      * - Mittente
135600030515     c                   if        DB0ccm <> *zeros
135700030515     c                   move      DB0ccm        Pccm
135800030515     c                   endif
135900030515     c                   eval      Prsm   =  DB0rsm
136000030515     c                   eval      Pinm   =  DB0inm
136100030515     c                   eval      Pcam   =  DB0cam
136200030515     c                   eval      Plom   =  DB0lom
136300030515     c                   eval      Pprm   =  DB0prm
136400030515     c                   eval      Pnzm   =  DB0nzm
136500030515      *   Mittente originale
136600030515if  1c                   if        DB0rmo <> *blanks
136700080430     c                   eval      Pkmo   =  'Mitt.Originale '
136800030515     c                   eval      Prmo   =  DB0rmo
136900030515e   1c                   endif
137000030515      *
137100070308      * - Filiale di Arrivo
137200030515     c                   move      DB0lna        Plna
137300030515      *
137400030515      * - Destinatario
137500030515if  1c                   if        DB0ccd <> *zeros
137600030515     c                   move      DB0ccd        Pccd
137700030515e   1c                   endif
137800030515     c                   eval      Prsd   =  DB0rsd
137900030515     c                   eval      Prse   =  DB0rd2
138000030515     c                   eval      Pind   =  DB0ind
138100030515     c                   eval      Pcad   =  DB0cad
138200030515     c                   eval      Plod   =  DB0lod
138300030515     c                   eval      Pprd   =  DB0prd
138400030515     c                   eval      Pnzd   =  DB0nzd
138500030515      *
138600030515      * - Codice tariffa
138700030515if  1c                   if        %subst(DB0tb1:1:1) = 'A'
138800030515      *   - nel box del destinatario se porto assegnato
138900030515     c                   move      DB0ctr        Pcta
139000030515x   1c                   else
139100030515      *   - nel box del mittente se porto franco
139200030515     c                   move      DB0ctr        Pctf
139300030515e   1c                   endif
139400030515      *
139500030515      * - Riferimenti mittente
139600080430     c                   evalr     Prmn   =  %char(DB0rmn)
139700030515     c                   eval      Prma   =  DB0rma
139800061107      * se bolla DPD recupero il riferimento da FIAR4 rcd "I"
139900130924     c***?(già fatto:)?  clear                   dsBL4I
140000060626     c                   if        *in71
140100061103     c                   exsr      RepAR4_I
140200060626     c                   endif
140300030515      *
140400030515      * - Nominativo & telefono
140500031124      *   (da FIAR5 / "GEN")
140600130924     c***?(già fatto:)?  exsr      RepAR5gen
140700031124     c                   eval      Pnom   =  §AR5ref
140800031124     c                   eval      Ptel   =  §AR5teld
140900030515      *
141000030515      * - Spedizione
141100030515      *   (numero: non se Rimesso al Mittente)
141200030515if  1c                   if        DB0ris <> 'M'
141300080430     c                   eval      Plnp   =  %editc(DB0lnp : 'X')
141400080430     c                   eval      Pnrs   =  %editc(DB0nrs : 'Z')
141500080430     c                   eval      Pnsp   =  %editc(DB0nsp : 'Z')
141600030515e   1c                   endif
141700030515      * . . Data spedizione
141800031124     c     *iso          movel     Wdsp          Data_EUR
141900080506     c                   eval      PdspA = %editc( %dec(Data_EUR) : 'Y' )
142000030515      *
142100030515      * - Colli: numero, peso, volume, flag tipo-volume
142200080430     c                   eval      PnclA  =  %editc(DB0ncl : 'Z')
142300080911      /free
142400080911           if  NOT *in71;
142500080911      /end-free
142600080430     c                   eval      PpkfA  =  %editc(DB0pkf : '4')
142700080430     c                   eval      PvlfA  =  %editc(DB0vlf : '4')
142800030515if  1c                   if            DB0fvf <> *blanks
142900030515     c                             and DB0ris <> 'M'
143000081021     c                   movel     DB0fvf        Pfvf
143100030515e   1c                   endif
143200080915      /free
143300080915           endif;
143400080915      /end-free
143500030515      *
143600030515      * - Natura merce
143700030515     c                   eval      Pnat   =  DB0nas
143800030515      *
143900030515      * - Servizio
144000100517      /free
144100100517         if  %subst(DB0tb1:1:1) = 'A'   or   DB0tb2 <> *blanks;
144200110117           exsr  sr_CheckORM_PreavvMail;
144300100517         endif;
144400100517      /end-free
144500100517      * . . Descrizione porto
144600130925      *     (NON se presente ORM con preavviso mail)
144700100517     c                   if        Not  $PreavvORM
144800030515     c                   eval      Pds1   =  DB0dcb
144900100517     c                   endif
145000030515      * . . Descrizione tipo servizio
145100030515if  1c                   if        DB0fst =  'S'
145200030515     c                   eval      Pds2   =  DB0dts
145300030515e   1c                   endif
145400030515      * . . Reso / Franco frontiera
145500030515sel 1c                   select
145600030515w   1c                   when      DB0fbr =  'S'
145700030515     c                   clear                   w008a
145800030515     c                   eval      w008a  =  '   RESO '
145900030515     c                   move      w008a         Pds2
146000030515w   1c                   when      DB0fbr =  'F'
146100030515     c                   clear                   w015a
146200030515     c                   eval      w015a  =  '   FR.FRONTIERA'
146300030515     c                   move      w015a         Pds2
146400030515e   1c                   endsl
146500030515      *
146600080430      * - Zona consegna
146700080430      *   (campo alfanumerico)
146800080430     c                   eval      Pznc   =  %editc(DB0znc : 'X')
146900030515      *
147000030515      * - Flag "*" (gestito a parte)
147100040223      *
147200040223      * - Flag "V" = colli di valore
147300040223if  1c                   if        §AR5bva   <> *blank
147400040223     c                   eval      Pbva   =  'V'
147500040223e   1c                   endif
147600030515      *
147700030515      * - Fermo deposito
147800030515if  1c                   if        DB0ffd <> *blanks
147900080430     c                   eval      Pgc2   =  'FERMO DEPOSITO '
148000030515e   1c                   endif
148100030515      *
148200070308      * - Segnacolli: filiale, serie, dal / al
148300030515if  1c                   if        DB0fls <> *zeros
148400080430     c                   eval      Ppos   =  %editc(DB0fls : 'X')
148500030515e   1c                   endif
148600040607      * - - se bolla con mamma considero la serie della mamma
148700070515if  1c                   if        $Mamma =  *on
148800080430     c                   eval      Psrs   =  %editc(wB0nrs : 'Z')
148900031015x   1c                   else
149000080430     c                   eval      Psrs   =  %editc(DB0nrs : 'Z')
149100031015e   1c                   endif
149200030515      * - Se colli non sequenziali: stampo "NON SEQUENZIALI" + Elenco
149300030515      *   Se collo cumulativo ....:   "     SCD "X" Num.Colli
149400030515      *   Se colli sequenziali ...:   "     SCD "/" SCA
149500030515      *   Se collo singolo .......:   "     SCD
149600031015sel 1c                   select
149700030515      * - - non sequenziali
149800030515w   1c                   when      DB0fns =  'S'
149900131008if  2c                   if        DB0ncl <= 2
150000050511     c                   exsr      ListaScNSd
150100131008x   2c                   else
150200081021     c                   eval      Pnsx   =  'NON SEQUENZIALI'
150300131008e   2c                   endif
150400030515      * - - singolo o sequenziali
150500030515      *     (non se rimesso al mittente)
150600030515w   1c                   when      DB0ris <> 'M'
150700080430     c                   eval      Pnsx   =  %editc(DB0ncd : 'Z')
150800030515if  2c                   if            DB0nca <> *zeros
150900030515     c                             and DB0nca <> DB0ncd
151000080430     c                   eval      Pnsx   =  %trimr(Pnsx) + ' - '
151100080430     c                                    +  %editc(DB0nca : 'Z')
151200030515e   2c                   endif
151300030515e   1c                   endsl
151400030515      *
151500131016      * - Note (1 e 2) e Particolarità (3, 4, 5 e 6)
151600030515     c                   exsr      SchPARTIC
151700131016e   1 *
151800030716if  1c                   if           $NP(1) <> *blanks
151900030716     c                             or $NP(2) <> *blanks
152000030716     c                             or $NP(3) <> *blanks
152100030716     c                             or $NP(4) <> *blanks
152200030716     c                             or $NP(5) <> *blanks
152300030716     c                             or $NP(6) <> *blanks
152400030710     c                   eval      Pnp1   =  $NP(1)
152500030710     c                   eval      Pnp2   =  $NP(2)
152600030710     c                   eval      Pnp3   =  $NP(3)
152700030710     c                   eval      Pnp4   =  $NP(4)
152800030710     c                   eval      Pnp5   =  $NP(5)
152900030710     c                   eval      Pnp6   =  $NP(6)
153000030716e   1c                   endif
153100030515      *
153200080401      * - Privato / Appuntamento / Uffici / Consegna Garantita
153300080401      *   (servizio consegna garantita x cliente Wuerth)
153400170413if  1c*//                if        DB0gva =  'P '
153500170413     c*//                movel     'PRIVATO'     Ppad
153600170413e   1c*//                endif
153700041012if  1c                   if        DB0gva =  'U '
153800041013     c                   eval      Ppad = 'CONSEGNA AD UFFICI'
153900041012e   1c                   endif
154000030515if  1c                   if           DB0tc1 =  'A'
154100030515     c                             or DB0tc2 =  'A'
154200030515     c                   move      'APPUNTAMENTO'Ppad
154300030515e   1c                   endif
154400080401if  1c                   if             DB0nrs = 40
154500090518     c                             and  DB0tsp = 'E'
154600080401     c                             and (DB0lnp = 043
154700080401     c                              or  DB0lnp = 006
154800080401     c                              or  DB0lnp = 063)
154900090304     c                   eval      Pds2 = 'CONSEGNA ESPRESSA +
155000090304     c                                     WÜRTH'
155100080401e   1c                   endif
155200030515      *
155300030515      * - Data e ora consegna richiesta
155400030515      *   e Flag "*" > x Data e/o Ora consegna richiesta
155500091116      *   (NO "Prima del" per il cliente TTL)
155600091116if  0c                   if        DB0ccm <> c_CliTTL  or
155700091116     c                             DB0dcr =  *zeros    or  DB0tcr <> 'P'
155800030515if  1c                   if        DB0dcr <> *zeros
155900030515     c                   eval      Past   =  '*'
156000030515     c     *iso          movel     DB0dcr        Data_EUR
156100030515     c     *eur          movel     Data_EUR      Pdcr
156200030515e   1c                   endif
156300030515if  1c                   if        DB0hcr <> *zeros
156400030515     c                   eval      Past   =  '*'
156500080430     c                   eval      Phcr   =  %editw(DB0hcr : '0 :  ')
156600030515e   1c                   endif
156700091116e   0c                   endif
156800030515      *
156900030515      * - Consegne particolari
157000030515     c                   eval      Pcp1   =  DB0dc1
157100030515     c                   eval      Pcp2   =  DB0dc2
157200030515      *
157300030515      * - Imponibile / IVA / Esenzione / Totale fattura e divisa
157400030515      *   (importi "arrotondati" a 2 decimali in stampa)
157500030515if  1c                   if        DB0ift <> *zeros
157600080515     c                   eval      Pkim   = 'Imponibile'
157700080430     c                   eval(h)   w0112  =  DB0imv
157800080430     c                   eval      Pimp   =  %editc(w0112 : 'K')
157900030515sel 2c                   select
158000030515w   2c                   when      DB0iva <> *zeros
158100080515     c                   eval      Pkie   = 'I.v.a.    '
158200030624     c                   eval(h)   w0112  =  DB0iva
158300080430     c                   eval      Piva   =  %editc(w0112 : 'K')
158400030515w   2c                   when         DB0cei <> *blanks
158500030515     c                             or DB0dce <> *blanks
158600130213     c                   eval      Pkie   = 'Esenzione '
158700130213     c                   eval      Pese   = '           ' +
158800130213     c                                      %subst(DB0dce : 1 : %len(§EIdes))
158900030515e   2c                   endsl
159000080515     c                   eval      Pktf   = 'Tot. Fatt.'
159100080430     c                   eval(h)   w0112  =  DB0ift
159200080430     c                   eval      Pift   =  %editc(w0112 : 'K')
159300030515if  2c                   if        DB0div <> §GEDcn
159400030515     c                   eval      Pdiv   =  DB0div
159500030515e   2c                   endif
159600030515e   1c                   endif
159700030515      *
159800030515      * - Contrassegno e relativa divisa
159900030515if  1c                   IF        DB0cas <> *zeros
160000030625      *
160100080430     c                   eval(h)   w0112  =  DB0cas
160200080430     c                   eval      Pcas   =  %editc(w0112 : 'K')
160300030515if  2c                   if        DB0vca <> §GEDcn
160400030515     c                   eval      Pdca   =  DB0vca
160500030515e   2c                   endif
160600030515     c                   exsr      CtrCAs
160700030624      * - - Nuovo controllo abilitazione c/assegno
160800030624     c                   reset                   TRUL21DS
160900030624     c                   eval      I21cbo =  DB0cbo
161000030624     c                   eval      I21tsp =  DB0tsp
161100030624     c                   eval      I21lnp =  DB0lnp
161200030624     c                   eval      I21nzm =  DB0nzm
161300030624     c                   eval      I21lna =  DB0lna
161400030624     c                   eval      I21nzd =  DB0nzd
161500130826     c                   if        DB0ccm >  *zero
161600130826     c                   eval      I21ksc =  DB0ccm
161700130826     c                   else
161800130826     c                   eval      I21ksc =  (DB0lnp * 10000 ) + 8888
161900130826     c                   endif
162000030624     c                   eval      I21tic =  DB0tic
162100030624if  2c                   if        DB0vca <> §GEDcn
162200030624     c                   eval      I21imp =  Wcas
162300030624     c                   eval      I21div =  §GEDcn
162400030624x   2c                   else
162500030624     c                   eval      I21imp =  DB0cas
162600030624     c                   eval      I21div =  DB0vca
162700030624e   2c                   endif
162800080430     c                   eval      I21ute =  SDSusr
162900040607     c                   eval      I21pgm =  SDSpgm
163000030624     c                   call      'TRUL21R'
163100030624     c                   parm                    TRUL21ds
163200030624      * - - Carico la tabella "Tipo incasso C/Assegno" (1A)
163300030624     c                   exsr      Tab_1A
163400030624     c                   movel     §1Ades        DB0dti
163500030624if  2c                   if            §1Adeo <> *blanks
163600030624     c                             and O21fca <> *blanks
163700030624     c                   movel     §1Adeo        DB0dti
163800030624e   2c                   endif
163900030624      * - - Imposto i campi di stampa
164000080430     c                   eval      Pcti   =  DB0tic
164100080430     c                   eval      Ptic   =  DB0dti
164200030625      *
164300030515e   1c                   ENDIF
164400030515      *
164500030515      * - Totale da esigere al ritiro o alla consegna e relativa divisa
164600030515if  1c                   IF        DB0ift >  *zeros
164700030625      *
164800030515if  2c                   if            DB0vca <> §GEDcn
164900030515     c                             and DB0vca <> *blanks
165000030515     c                   eval      Pdie   =  DB0div
165100030515e   2c                   endif
165200030515      *   Controllo se C/Assegno da sommare al totale da esigere o no:
165300030515      *   se c'è il totale fattura: metto anche il il totale da esigere.
165400030515      *   (importi "arrotondati" a 2 decimali in stampa)
165500030515sel 2c                   Select
165600110204      *   Non ho C/Assegno
165700101213w   2c                   When      DB0cas  = *zeros
165800030624     c                   eval(h)   w0112  =  DB0ift
165900030624     c                   eval      Pies   =  %editc(w0112 : 'K')
166000030625     c                   clear                   Pie1
166100030625     c                   clear                   Pdi1
166200030515      *   C/Assegno sommabile
166300030515w   2c                   When      DB0ste =  'S'    and $CAeur = *on
166400030515sel 3c                   select
166500030515      *   - c/assegno in EURO
166600030515w   3c                   when      DB0div =  §GEDcn and DB0vca =  §GEDcn
166700030624     c                   eval(h)   w0112  =  DB0ift + DB0cas
166800030624     c                   eval      Pies   =  %editc(w0112 : 'K')
166900030624     c                   clear                   Pie1
167000030624     c                   clear                   Pdi1
167100030515      *   - c/assegno in DIVISA
167200030515w   3c                   when          DB0div =  §GEDcn
167300030515     c                             and DB0vca <> §GEDcn
167400030624     c                   eval(h)   w0112  =  DB0ift + Wcas
167500030624     c                   eval      Pies   =  %editc(w0112 : 'K')
167600030624     c                   clear                   Pie1
167700030624     c                   clear                   Pdi1
167800030515e   3c                   endsl
167900100218      *   C/Assegno non sommabile
168000030515x   2c                   Other
168100030624     c                   eval(h)   w0112  =  DB0cas
168200030624     c                   eval      Pie1   =  %editc(w0112 : 'K')
168300100218     c*//                eval      Pdi1   =  DB0vca
168400030624     c                   eval(h)   w0112  =  DB0ift
168500030624     c                   eval      Pies   =  %editc(w0112 : 'K')
168600100218     c*//                eval      Pdie   =  DB0div
168700030515e   2c                   Endsl
168800030709      *
168900030709e   1c                   ENDIF
169000030515      *
169100030515      * - Numero e data fattura
169200030515if  1c                   if        DB0nft <> *zeros
169300080430     c                   eval      Pkft   =  'Fatt.'
169400080430     c                   eval      Pnft   =  %editc(DB0nft : 'Z')
169500130205     c                   eval      Pfiv   = '(' + %editc(wB0fiv:'X') +
169600130205     c                                      ')'
169700030515if  2c                   if        DB0dft >  *zeros
169800080430     c                   eval      Pkdf   =  ' del '
169900030515     c     *iso          movel     DB0dft        Data_EUR
170000130213     c     *dmy          movel     Data_EUR      Pdft
170100030515e   2c                   endif
170200030515e   1c                   endif
170300030515      *
170400030515      * Memorizzo il codice tariffa come numero di copie da stampare
170500030515      *   (per Rimesso Mittente - non per DPD o FEDEX)
170600030515if  1c                   if        DB0ris = 'M'  and  *in70
170700030515     c                   eval      Wctr   =  DB0ctr
170800030515e   1c                   endif
170900030515      *
171000030515     c                   ENDSR
171100030515      *
171200030515      *---------------------------------------------------------------*
171300050217      *?Inschieramento delle Note e delle Particolarità              ?*
171400030515      *---------------------------------------------------------------*
171500030515     c     SchPARTIC     BEGSR
171600030515      *
171700130924     c***?(già fatto:)?  reset                   $NoDesPartic
171800130924     c***?(già fatto:)?  clear                   $NP
171900130924     c***?(già fatto:)?  clear                   W_CoOr
172000030515     c                   clear                   XX
172100030710      *
172200030710      * Imposto eventuali note per prime
172300030710if  1c                   if        DB0nt1 <> *blanks
172400030710     c                   add       1             xx
172500030710     c                   movel     DB0nt1        $NP(xx)
172600030710e   1c                   endif
172700030710if  1c                   if        DB0nt2 <> *blanks
172800030710     c                   add       1             xx
172900030710     c                   movel     DB0nt2        $NP(xx)
173000030710e   1c                   endif
173100030515      *
173200030515      * Le particolarità possono essere lunghe fino a 45 char.
173300030515      *   Se i primi 30 (della 1ª riga a disposizione) non dovessero
173400030515      *   bastare, il resto lo si mette nella riga successiva.
173500030710      *   Se, invece, non risultano decodificate, si mettono solo i
173600030710      *   codici sulla stessa riga.
173700030710      *   Se uno solo risulta decodificato, esso va su una riga a
173800030710      *   parte (vedi flag "$NoDesPartic": *off = serve riga nuova
173900030710      *                                    *on  = ok riga procedente)
174000030515      *
174100030515      * . . Particolarità Giacenza
174200030515if  1c                   IF        DB0gga <> *blanks
174300030515     c                   movel     'Q'           WTpPar
174400030515     c                   movel     DB0gga        WCdPar
174500030515     c                   exsr      TABxPAR
174600030710if  2c                   if            TBLuni       = *blanks
174700030710     c                             and $NoDesPartic = *on
174800030710     c                   eval      $NP(xx) = %trim($NP(xx))
174900030710     c                                     + '   ' + WCdPar
175000030710x   2c                   else
175100030710if  3c                   if        TBLuni       = *blanks
175200030710     c                   eval      $NoDesPartic = *on
175300030710x   3c                   else
175400030710     c                   eval      $NoDesPartic = *off
175500030710e   3c                   endif
175600030515     c                   add       1             xx
175700100517     c                   movel     w053a         $NP(xx)
175800100517if  3c                   if        %subst(w053a : %len($NP(xx))+1)
175900030515     c                              <> *blanks
176000030515     c                   add       1             xx
176100030515     c                   eval      $NP(xx) = '   ' +
176200100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
176300030710e   3c                   endif
176400030515e   2c                   endif
176500030515e   1c                   ENDIF
176600030515      *
176700030515      * . . Particolarità Consegna
176800030515if  1c                   IF        DB0gma <> *blanks
176900030515     c                   movel     'R'           WTpPar
177000030515     c                   movel     DB0gma        WCdPar
177100030515     c                   exsr      TABxPAR
177200030710if  2c                   if            TBLuni       = *blanks
177300030710     c                             and $NoDesPartic = *on
177400030710     c                   eval      $NP(xx) = %trim($NP(xx))
177500030710     c                                     + '   ' + WCdPar
177600030710x   2c                   else
177700030710if  3c                   if        TBLuni       = *blanks
177800030710     c                   eval      $NoDesPartic = *on
177900030710x   3c                   else
178000030710     c                   eval      $NoDesPartic = *off
178100030710e   3c                   endif
178200030515     c                   add       1             xx
178300100517     c                   movel     w053a         $NP(xx)
178400100517if  3c                   if        %subst(w053a : %len($NP(xx)) + 1)
178500030515     c                              <> *blanks
178600030515     c                   add       1             xx
178700030515     c                   eval      $NP(xx) = '   ' +
178800100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
178900030710e   3c                   endif
179000030515e   2c                   endif
179100030515e   1c                   ENDIF
179200030515      *
179300030515      * . . Particolarità Varie
179400030717      *     - questa descrizione va messa nell'apposito campo PPAD
179500041015      *       se è "PRIVATO" idem se è "CONSEGNA AD UFFICI"
179600030717      *     - se 'B' (bancali da rendere al mittente) comparirà un
179700030717      *       apposito box
179800030717      *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
179900030717      *       comparirà un apposito box, un'apposita nota e l'indica-
180000030717      *       zione nella copia per il destinatario.
180100030717      *     ...Potrebbero anche non esserci più elementi di schiera
180200030717      *     disponibili [xx >= %elem($NP)] !
180300030709if  1c                   IF            DB0gva <> *blanks
180400030717     c                             and DB0gva <> 'B '
180500030717     c                             and DB0gva <> 'O '
180600030709     c                             and DB0gva <> 'P '
180700041015     c                             and DB0gva <> 'U '
180800030515     c                   movel     'S'           WTpPar
180900030515     c                   movel     DB0gva        WCdPar
181000030515     c                   exsr      TABxPAR
181100030710if  2c                   if            TBLuni       = *blanks
181200030710     c                             and $NoDesPartic = *on
181300030710     c                   eval      $NP(xx) = %trim($NP(xx))
181400030710     c                                     + '   ' + WCdPar
181500030710x   2c                   else
181600030710if  3c                   if        TBLuni       = *blanks
181700030710     c                   eval      $NoDesPartic = *on
181800030710x   3c                   else
181900030710     c                   eval      $NoDesPartic = *off
182000030710e   3c                   endif
182100030717if  3c                   if            xx < %elem($NP)
182200030709     c                   add       1             xx
182300100517     c                   movel     w053a         $NP(xx)
182400100517if  4c                   if        %subst(w053a : %len($NP(xx)) + 1)
182500030709     c                              <> *blanks
182600030717     c                             and xx < %elem($NP)
182700030709     c                   add       1             xx
182800030709     c                   eval      $NP(xx) = '   ' +
182900100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
183000030717e   4c                   endif
183100030717e   3c                   endif
183200030709e   2c                   endif
183300030515e   1c                   ENDIF
183400030717      *
183500030717      * Imposto il numero originale di colli componenti la spedizione
183600030717      * e se ho ancora spazio lo memorizzo in "Note e Particolarità"
183700030717if  1c                   if        %subst(DB0gva:1:1) = 'O'
183800030717     c                   exsr      RepAR5bnb
183900030717if  2c                   if        W_CoOr  <> *blanks
184000030717     c                             and xx  <  %elem($NP)
184100030717     c                   add       1             xx
184200030717     c                   eval      $NP(xx) =  W_CoOr
184300030717e   2c                   endif
184400030717e   1c                   endif
184500030515      *
184600030515     c                   ENDSR
184700070319      *
184800070319      *---------------------------------------------------------------*
184900070319      *?Inschieramento delle Consegne Parziali precedenti            ?*
185000070319      *---------------------------------------------------------------*
185100070319     c     SchConsParz   BEGSR
185200070319      *
185300130924     c***?(già fatto:)?  reset                   $CP
185400130924     c***?(già fatto:)?  reset                   $CPdcm
185500070319     c                   clear                   XX
185600070319      *
185700070319if  2c                   if        NOT %open(FNART01L)
185800101213     c                   open(e)   FNART01L
185900101213      /free
186000101213             if  %error;
186100101213               exsr  sr_ErrOpenFile;
186200101213             endif;
186300101213      /end-free
186400070319e   2c                   endif
186500070319      *
186600070514if  2c                   if        $Mamma = *off
186700070319     c     KLDV          setll     FNART000
186800070319     c     KLDV          reade     FNART000
186900070514x   2c                   else
187000070514     c     KLBL          setll     FNART000
187100070514     c     KLBL          reade     FNART000
187200070514e   2c                   endif
187300070319      *
187400070319do  1c                   DOW       NOT  %eof(FNART01L)
187500070319      *
187600070319if  2c                   If             ARTatb = *blanks
187700070319     c                             and  ARTdcm > *zeros
187800070319      *
187900070319     c                   eval      *in31  = *off
188000070319     c                   eval      xx     = 1
188100070319     c     ARTdcm        lookup    $CPdcm(xx)                             31
188200070319if  3c                   if        NOT  *in31
188300070319     c                   eval      xx     = 1
188400070319     c     *zeros        lookup    $CPdcm(xx)                             31
188500070319if  4c                   if        *in31
188600140304     c*//                eval      $CP(xx)     = *zeros
188700140304     c                   clear                   Elem_CP
188800140304     c                   movel     Elem_CP       $CP(xx)
188900070319e   4c                   endif
189000070319e   3c                   endif
189100070319if  3c                   if        *in31
189200070319     c                   eval      $CPdcm(xx)  = ARTdcm
189300070319     c                   movel     $CP(xx)       Elem_CP
189400070319     c                   eval      WCPdcm      = ARTdcm
189500070319     c                   eval      WCPncl      = WCPncl + 1
189600070319     c                   movel     Elem_CP       $CP(xx)
189700070319e   3c                   endif
189800070319      *
189900070319e   2c                   EndIf
190000070319      *
190100070514if  2c                   if        $Mamma = *off
190200070319     c     KLDV          reade     FNART000
190300070514x   2c                   else
190400070514     c     KLBL          reade     FNART000
190500070514e   2c                   endif
190600070319      *
190700070319e   1c                   ENDDO
190800070319      *
190900140514if  0c                   If        xx     > *zero
191000140513      * Reperimento nominativo firmatario   &
191100140514     c                   exsr      sr_Firmatario
191200070319      * Ordinamento schiera per data
191300070319if  1c                   if        xx     > 1
191400070319     c                   sorta                   $CP
191500070319e   1c                   endif
191600140514e   0c                   EndIf
191700070319      *
191800070319     c                   ENDSR
191900140303      *
192000140303      /free
192100140303
192200140303       //--------------------------------------------------------------
192300140303       //?Reperimento nome firmatario                                  ?
192400140303       //--------------------------------------------------------------
192500140303       BEGSR  sr_Firmatario;
192600140303
192700140303if  1    if  NOT  %open(FNEVB22L);
192800140303           open  FNEVB22L;
192900140303e   1    endif;
193000140303
193100140303         clear  keyFNEVB22;
193200140303         k_EVB2aas = DB0aas;
193300140303         k_EVB2lnp = DB0lnp;
193400140303         k_EVB2nrs = DB0nrs;
193500140303         k_EVB2nsp = DB0nsp;
193600140303         k_EVB2cev = 'P  ';
193700140303         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
193800140303         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
193900140303         DoW  Not  %eof(FNEVB22L);
194000140303           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
194100140304             yy = %lookup( EVB2dev : $CPdcm );
194200140304             if  yy > *zero;
194300140304               Elem_CP = $CP(yy);
194400140303               if  WCPocm  <= EVB2hev;
194500140303                 WCPocm  = EVB2hev;
194600140303                 WCPfir  = EVB2dati;
194700140304                 $CP(yy) = Elem_CP;
194800140303               endif;
194900140303             endif;
195000140303           endif;
195100140304           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
195200140303         EndDo;
195300140303
195400140303       ENDSR;
195500140303
195600140303      /end-free
195700031124      *
195800031124      *---------------------------------------------------------------*
195900050217      *?Reperimento Nominativo e Telefono destinatario da FIAR5/GEN  ?*
196000031124      *---------------------------------------------------------------*
196100031124     c     RepAR5gen     BEGSR
196200031124      *
196300031124     c                   clear                   dAR5gen
196400031124      *
196500031124     c                   eval      AR5trd = 'GEN'
196600031127     c     K05AR501      chain     FIAR5000
196700031124      *
196800101213if  1c                   if            %found(FIAR501L)
196900040301     c                             and AR5atb =  *blanks
197000031124     c                   movel     AR5uni        dAR5gen
197100031124e   1c                   endif
197200031124      *
197300031124     c                   ENDSR
197400030717      *
197500030717      *---------------------------------------------------------------*
197600050217      *?Reperimento Numero Colli Originale da FIAR5/BNB              ?*
197700030717      *---------------------------------------------------------------*
197800030717     c     RepAR5bnb     BEGSR
197900030717      *
198000030717     c                   clear                   dAR5bnb
198100130924     c***?(già fatto:)?  clear                   W_CoOr
198200030717      *
198300030717     c                   eval      AR5trd = 'BNB'
198400030717     c     K05AR501      chain     FIAR5000
198500030717      *
198600101213if  1c                   if            %found(FIAR501L)
198700040301     c                             and AR5atb =  *blanks
198800030717     c                   movel     AR5uni        dAR5bnb
198900030717if  2c                   if        §AR5bcor <> *zeros
199000030717     c                   eval      W_CoOr   =  C_CoOr
199100030717     c                                      +  %editc(§AR5bcor : 'K')
199200030717e   2c                   endif
199300030717e   1c                   endif
199400030717      *
199500030717     c                   ENDSR
199600030515      *
199700030515      *---------------------------------------------------------------*
199800051129      *?Decodifiche condizionate in Lettera di Vettura               ?*
199900030515      *---------------------------------------------------------------*
200000110208     c     sr_DecodLDV   BEGSR
200100030515      *
200200030515      * Richiesta firma per ricevuta della merce senza riserve
200300030515      * (e NON  quella  per quietanza) => *in35 + not *in36
200400030515     c                   movea     '10'          *in(35)
200500070307      *
200600070307      * Intestazione moduli
200700090619      *
200800090619      /free
200900121018
201000121018         // -?Indicazione della Partiva IVA o del Cod. Fiscale del Destinatario?
201100121018         //  ?se Porto Assegnato con Fattura immediata?
201200121018         //  ?- NON se destinatario estero -?
201300121018             // -?con fattura?
201400121018         IF  (DB0nft <> *zero  or   DB0ift <> *zero)   AND     (
201500121018             // - -?P.A. (con fattura immediata)?
201600121018             //    ?(con destinatario italiano)?
201700121018             (%subst(DB0tb1:1:1) = 'A'  and  DB0nzd  = *blank)  or
201800121018             // - -?con 2ª bolla?
201900121018             //    ?(con destinatario italiano)?
202000131203              (DB0tb2 <> *blank         and  DB0nzd  = *blank) );
202100121018           select;
202200131010             // ·?Esiste Partita Iva NON FITTIZIA?
202300131010             when  wB0piv <> *blank  and
202400121026                   %scan('$$' : wB0piv) = *zero;
202500121018               PpivD = 'Partita Iva: ' + wB0piv;
202600131010             // ·?Esiste Codice Fiscale?
202700131010             when  wB0cfi <> *blank;
202800121018               PpivD = 'Codice Fiscale: ' + wB0cfi;
202900121018           endsl;
203000121018         ENDIF;
203100110204
203200110204         // -?Consegna al mattino (tempi consegna = 12 ore)?
203300110204         //  ?(NON per Würth)?
203400110204         If  §VPOrcm <= wDate and
203500090619             DB0tsp   = 'E'   and
203600090619            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
203700090619                                   DB0lnp <> 006   and
203800090619                                   DB0lnp <> 063));
203900090619           clear  TNSD99ds;
204000101213           D98tbo = 'A';
204100090619           D98aas = DB0aas;
204200090619           D98lnp = DB0lnp;
204300090619           D98nrs = DB0nrs;
204400090619           D98nsp = DB0nsp;
204500090619           tnsd99r ( TNSD99ds );
204600120910           if  D98tcc = 12;
204700090619             %subst(Pds2 : %len(Pds2) - 1) = ' M';
204800090619           endif;
204900110204         EndIf;
205000110204
205100110204         // -?"NON UTILIZZARE PER LA CONSEGNA"?
205200110204         //  ?(stampa della sola copia per il vettore)?
205300110204         if  $No_x_Cons = *on;
205400110204           *in35 = *off;
205500110204           //*in36 = *off;
205600110204           // -?costante da stampare se LDV recapito c/assegni stampata?
205700110204           //  ?dal pgm "stampa DDT NO in arrivo"?
205800110204           //Pnux   =  ' NON UTILIZZARE PER LA CONSEGNA';
205900110204         endif;
206000110204
206100110204         // -?Decodifica "Riserva Consegna" (se c'è)?
206200110204         clear  ds2A;
206300110204         If  ILVriserv <> *blank;
206400110204           TBLcod = '2A';
206500110204           TBLkey = ILVriserv;
206600110207           chain  K03TAB  TABEL;
206700110204           if  %found(TABEL00F);
206800110204             ds2A = TBLuni;
206900110204           endif;
207000110204         EndIf;
207100140514
207200140514         // -?Reperimento consegne parziali precedenti da FNART?
207300140514         If  $ConsParz;
207400140514           exsr  SchConsParz;
207500140514           if  xx < 1;
207600140514             $ConsParz = *off;
207700140514           endif;
207800140514         EndIF;
207900110204
208000110204         // -?Costante "PER RICEVUTA DELLA MERCE..."?
208100130716         exsr  sr_PerRicevutaDellaMerce;
208200110204
208300090619      /end-free
208400030515      *
208500130411      * Verifico se Packing-List e/o PDF
208600130411     c                   select
208700130411     c                   when      $§PDF  = *on
208800130411     c                   eval      Ppkl   =  'PACKING LIST PDF'
208900130411     c                   when      $FIAR8 =  *on
209000130411     c                   eval      Ppkl   =  'PACKING LIST    '
209100130411     c                   endsl
209200030515      *
209300030515      * Flag "*" > x Consegne particolari
209400030515     c                   if        DB0tc1 =  'A'  or  DB0tc2 = 'A'
209500030515     c                   eval      Past   =  '*'
209600030515     c                   endif
209700030515      *
209800030515      * C/ASSEGNO - - - - - - - - - - - - - - - - - - - - - - - - - - *
209900030515if  1c                   IF        DB0cas <> *zeros
210000030515      * - Flag "*" > x C/Assegno
210100030904if  2c                   if        O21fca <> *blanks
210200030904     c                   movel     '*'           Past
210300030515e   2c                   endif
210400030624      * - Imposto i campi di stampa
210500080430     c                   eval      Pcti   =  DB0tic
210600080430     c                   eval      Ptic   =  DB0dti
210700030515e   1c                   ENDIF
210800030515      *
210900040607      * TIPO LDV  - - - - - - - - - - - - - - - - - - - - - - - - - - *
211000030515      * - Se Bolla in Assegnato: stampo una "A" vicino all'asterisco
211100030515      * e se Bolla in Assegnato Senza Fattura: stampo "SEGUE FATTURA"
211200030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
211300030515     c                             or        DB0tb2     <> *blanks
211400030515     c                   eval      Pass   =  'A'
211500030515if  2c                   if        DB0nft =  *zeros
211600080430     c                   if        Pkie   =  *blanks
211700080430     c                   evalr     Pkie   =  'SEGUE'
211800080430     c                   eval      Piva   =  ' FATTURA'
211900080430     c                   else
212000080430     c                   eval      Piva   =  'SEGUE FATTURA'
212100080430     c                   endif
212200030515e   2c                   endif
212300030515e   1c                   endif
212400030515      *
212500030515      * DECODIFICA TIPO CONSEGNA RICHIESTA  - - - - - - - - - - - - - *
212600051129if  1c                   IF            DB0dcr >  *zeros
212700051129     c                             or  DB0hcr >  *zeros
212800051129     c                   clear                   FNLV80ds
212900051129     c                   eval      ILV80aas   =  DB0aas
213000051129     c                   eval      ILV80lnp   =  DB0lnp
213100051129     c                   eval      ILV80nrs   =  DB0nrs
213200051129     c                   eval      ILV80nsp   =  DB0nsp
213300051129     c                   eval      ILV80mgs   =  DB0mgs
213400051129     c                   eval      ILV80tc1   =  DB0tc1
213500051129     c                   eval      ILV80tc2   =  DB0tc2
213600051129     c                   eval      ILV80dcr   =  DB0dcr
213700051129     c                   eval      ILV80hcr   =  DB0hcr
213800051129     c                   eval      ILV80tcr   =  DB0tcr
213900051129     c                   call      'FNLV80R'
214000051129     c                   parm                    KPJBA
214100051129     c                   parm                    FNLV80ds
214200051129sel 2c                   Select
214300051129      * errore
214400051129w   2c                   When      OLV80err  <>  *blanks
214500051129      * consegna tassativa (per ora solo ai supermercati)
214600051129w   2c                   When      OLV80tat   =  'S'
214700081021     c                   movel     OLV80tcrd     Ptdr
214800091116      * NO "Prima del" per il cliente TTL
214900091116w   2c                   When      DB0ccm     = c_CliTTL   and
215000091116     c                             DB0dcr    <> *zeros     and  DB0tcr = 'P'
215100051129      * prima/dopo il/delle
215200051129      * (il giorno e/o l'ora di consegna richiesta sono già impostati
215300110208      *  nella subr. "sr_TipoLDV")
215400081022x   2c                   Other
215500081021     c                   movel     OLV80tcrd     Ptdr
215600081021e   2c                   Endsl
215700051129e   1c                   ENDIF
215800030515      *
215900030515      * DECODIFICA DEI TURNI DI CHIUSURA  - - - - - - - - - - - - - - *
216000060921      * (SE?non?è "Fermo Deposito")
216100030515      * 1° turno di chiusura
216200060921if  1c                   IF             DB0gc1 <> *blanks
216300060921     c                             and  DB0ffd  = *blanks
216400030515      * - Flag "*" > Turno di chiusura
216500030515     c                   eval      Past   =  '*'
216600030515if  2c                   If        %subst(DB0gc1:1:1) = *blank
216700030515      * - Nessun giorno specificato - Solo 'M' o 'P'
216800130924     c***?già così:?     clear                   Pgc1
216900030515if  3c                   if        DB0gc1 =  ' M'
217000030515     c                   eval      Pgc1   =  %trim($Gio(8))
217100030515x   3c                   else
217200030515     c                   eval      Pgc1   =  %trim($Gio(9))
217300030515e   3c                   endif
217400030515x   2c                   Else
217500030515      * - Giorno specificato
217600030515     c                   movel     DB0gc1        w001a
217700030515     c                   eval      XX     =  1
217800030515     c                   eval      *in31  =  *off
217900030515     c     w001a         lookup    $GgS(xx)                               31
218000030515     c   31              movel     $Gio(xx)      Pgc1
218100030515      *   + 'M' o 'P'
218200030515sel 3c                   select
218300030515w   3c                   when      %subst(DB0gc1:2:1) = 'M'
218400030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
218500030515     c                                    +  %trim($Gio(8))
218600030515w   3c                   when      %subst(DB0gc1:2:1) = 'P'
218700030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
218800030515     c                                    +  %trim($Gio(9))
218900030515e   3c                   endsl
219000030515e   2c                   Endif
219100030515e   1c                   ENDIF
219200030515      * 2° turno di chiusura
219300060921if  1c                   IF             DB0gc2 <> *blanks
219400060921     c                             and  DB0ffd  = *blanks
219500030515      * - Flag "*" > Turno di chiusura
219600030515     c                   eval      Past   =  '*'
219700030515if  2c                   If        %subst(DB0gc2:1:1) = *blank
219800030515      * - Nessun giorno specificato - Solo 'M' o 'P'
219900130924     c***?già così:?     clear                   Pgc2
220000030515if  3c                   if        DB0gc2 =  ' M'
220100080515     c                   eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
220200080515     c                                                = %trim($Gio(8))
220300030515x   3c                   else
220400080515     c                   eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
220500080515     c                                                = %trim($Gio(9))
220600030515e   3c                   endif
220700030515x   2c                   Else
220800030515      * - Giorno specificato
220900030515     c                   movel     DB0gc2        w001a
221000030515     c                   eval      XX     =  1
221100030515     c                   eval      *in31  =  *off
221200030515     c     w001a         lookup    $GgS(xx)                               31
221300080515     c   31              eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
221400080515     c                                                = %trim($Gio(xx))
221500030515      *   + 'M' o 'P'
221600030515sel 3c                   select
221700030515w   3c                   when      %subst(DB0gc2:2:1) = 'M'
221800080513     c                   eval      Pgc2   =  %trimr(Pgc2) + ' '
221900030515     c                                    +  %trim($Gio(8))
222000030515w   3c                   when      %subst(DB0gc2:2:1) = 'P'
222100080513     c                   eval      Pgc2   =  %trimr(Pgc2) + ' '
222200030515     c                                    +  %trim($Gio(9))
222300030515e   3c                   endsl
222400030515e   2c                   Endif
222500030515e   1c                   ENDIF
222600080523      /free
222700080523       // ________________________________________
222800080523       // Impostazione nominativo del destinatario
222900080523       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
223000110214         exsr  RepAR4_1;
223100080523       // _______________________________________
223200080523       // Impostazione data ed ora della consegna
223300080523       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
223400110228         //?ATTENZIONE:?il "RESET" di un campo di tipo "d" (data)?
223500110228         //?riporta in tale campo la *date, NON quella impostata?
223600110228         //?come "inz(...)" della specifica "d" !!!!!!!!!!!!!!!!?
223700110228         Data_EUR = *loval;
223800110209         clear  w0080;
223900110209         select;
224000110209           when  wB0dcm > *zero;
224100110209             Data_EUR = %date( wB0dcm : *iso );
224200110209           when  wB0dcp > *zero;
224300110209             Data_EUR = %date( wB0dcp : *iso );
224400110209         endsl;
224500110209         if  Data_EUR > *loval;
224600110209           w0080 = ( ( %subdt(Data_EUR : *days) * 1000000 ) +
224700110209                     ( %subdt(Data_EUR : *months) * 10000 ) +
224800110209                     ( %subdt(Data_EUR : *years) ) );
224900110209         endif;
225000150430         if  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
225100081103           Pfedkcd  = %editc( w0080 : 'Y' );
225200120702           if  dILVlibero.§ILVhmp > *zero;
225300081103             Pfedkcd = %trimr( Pfedkcd ) + '  '
225400120702                     + %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':'
225500120702                     + %subst( dILVlibero.§ILVhmp : 3 : 2 );
225600081103           endif;
225700081103         else;
225800081103           Pkcd  = %editc( w0080 : 'Y' );
225900120702           if  dILVlibero.§ILVhmp > *zero;
226000081103             Pkcd = %trimr( Pkcd ) + '   '
226100120702                  + %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':'
226200120702                  + %subst( dILVlibero.§ILVhmp : 3 : 2 );
226300081103           endif;
226400081103         endif;
226500080523      /end-free
226600071114      *
226700071114      * ALTRI DATI/FLAG/... - - - - - - - - - - - - - - - - - - - - - *
226800030515      *
226900071114      * Se pgm richiamato x stampa LdV in arrivo:
227000030515if  1c                   IF        DB0fl1 =  'A'
227100071114      * - imposto l'"*" in base al parametro ricevuto.
227200030515if  2c                   if        DB0fl2 =  '*'
227300030515     c                   eval      Past   =  '*'
227400030515x   2c                   else
227500030515     c                   clear                   Past
227600030515e   2c                   endif
227700071114      * - idem per flag di bolla non abilitata
227800030515     c                   eval      Pdis   =  DB0fl5
227900030515e   1c                   endif
228000030515      *
228100030515     c                   ENDSR
228200080430      *
228300080430      /free
228400130716
228500130716       //--------------------------------------------------------------
228600130716       //?Impostazione "PER RICEVUTA DELLA MERCE ..."                  ?
228700130716       //--------------------------------------------------------------
228800130716       BEGSR  sr_PerRicevutaDellaMerce;
228900130716
229000130716         Select;
229100130716
229200130716           // -?danon?stampare se?
229300130716           // ·?non utilizzare per la consegna  (NOT *in35)?
229400130716           When  NOT *in35;
229500130716             //clear  Pkrm;       ?(già fatto!)?
229600140108             //clear  Pkrm2;      ?(già fatto!)?
229700130716
229800130716           // -?costante per "DPD"   (come standard)?
229900130716           //         ?o per "FedEx" (come standard)?
230000130716           When  *in71  or  *in72;
230100130716             Pkrm = 'PER RICEVUTA DELLA MERCE';
230200130716             // -?senza riserve?
230300130716             if  ILVriserv = *blank;
230400130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
230500130716             // -?con Riserva Consegna?
230600130716             else;
230700130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
230800130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
230900130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
231000130716               endif;
231100130716             endif;
231200130716
231300130716           // -?costante per cliente?
231400130716           //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
231500140513           When  DB0ccm = c_CliCLN  or  DB0ccm = c_CliLN_2;
231600130716             exsr  sr_RepDdtCLN;
231700130716             Pkrm = 'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
231800130716                     DDT '
231900130716                  + %trimr( %editc (WddtCLN : 'Z') )
232000130716                  + ' DEL '
232100130716                  + %editc (WdspCLN : 'Y')
232200130716                  + ' CON RISERVA DI CONTROLLO COME CONTRATTO';
232300130716             if  Wdsp < 20101001;
232400130716               Pkrm = %trimr(Pkrm) + ' C.L.N.';
232500130716             else;
232600130716               Pkrm = %trimr(Pkrm) + ' L.N. S.R.L.';
232700130716             endif;
232800130716
232900130716           // -?costante "standard" se "DDT sì"?
233000130716           When  §VPOrsr <= wDate  and  DB0bam = 'S';
233100130716             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
233200130716                     TRASPORTO';
233300130716             // -?senza riserve?
233400130716             if  ILVriserv = *blank;
233500130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
233600130716             // -?con Riserva Consegna?
233700130716             else;
233800130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
233900130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
234000130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
234100130716               endif;
234200130716             endif;
234300130716
234400130716           // -?costante "standard" se "DDT no" + Packing-List?
234500161012           When  DB0bam = *blank  and  ($FIAR8 = *on  or  $§PDF = *on);
234600130716             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
234700130716                     TRASPORTO';
234800130716             // -?senza riserve?
234900130716             if  ILVriserv = *blank;
235000130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
235100130716             // -?con Riserva Consegna?
235200130716             else;
235300130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
235400130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
235500130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
235600130716               endif;
235700130716             endif;
235800130716
235900130716           // -?costante "standard" altrimenti?
236000130716           Other;
236100130716             Pkrm = 'PER RICEVUTA DELLA MERCE';
236200130716             // -?senza riserve?
236300130716             if  ILVriserv = *blank;
236400130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
236500130716             // -?con Riserva Consegna?
236600130716             else;
236700130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
236800130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
236900130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
237000130716               endif;
237100130716             endif;
237200130716
237300130716         EndSl;
237400130716
237500130716         // -?costante aggiuntiva per cliente?
237600130716         //  ?"S G Soluzioni Logistiche"?
237700130716         If  DB0ccm = c_CliSG   or   DB0ccm = c_CliSG_2;
237800130716
237900130716           if  DB0nas <> *blank   and   DB0nas <> c_NoContr;
238000130716             Pkrm2 = 'INCLUDE CONTRATTO - '
238100130716                   + %trim(DB0nas);
238200130716           endif;
238300130716           if  DB0rmo <> *blank;
238400130716             if  Pkrm2 <> *blank;
238500130716               Pkrm2 = %trimr(Pkrm2)
238600130716                     + ' - MATR TV: '
238700130716                     + %trim(DB0rmo);
238800130716             else;
238900130716               Pkrm2 = 'MATR TV: '
239000130716                     + %trim(DB0rmo);
239100130716             endif;
239200130716           endif;
239300130716
239400130716         EndIf;
239500140108
239600140108         // -?Consegne Parziali Precedenti?
239700140108         If  $ConsParz = *on  and  wB0dcm = *zero;
239800140108
239900140108           Select;
240000140110             When  Pkrm2 = *blank;
240100140110               Pkrm2 = %subst( Pkrm2 : 1 : 110 ) + ' PARZIALE';
240200140108             When  %subst( Pkrm : 111 ) = *blank;
240300140108               Pkrm = %subst( Pkrm : 1 : 110 ) + ' PARZIALE';
240400140108             When  %subst( Pkrm2 : 111 ) = *blank;
240500140108               Pkrm2 = %subst( Pkrm2 : 1 : 110 ) + ' PARZIALE';
240600140108           EndSl;
240700140108
240800140108         EndIf;
240900130716
241000130716       ENDSR;
241100090619
241200090619       //--------------------------------------------------------------
241300130716       //?Reperimento Numero e Data del DDT originale del mittente     ?
241400130716       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
241500090619       //--------------------------------------------------------------
241600090619       BEGSR  sr_RepDdtCLN;
241700090619
241800130716         // -?Verifica se la spedizione ha mamma?
241900090619         if  not %open(FNLBL01L);
242000101213           open(e)  FNLBL01L;
242100101213           if  %error;
242200101213             exsr  sr_ErrOpenFile;
242300101213           endif;
242400090619         endif;
242500090619         chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
242600090619
242700090619         Select;
242800090619
242900130716           // -?Con Mamma: reperimento dati DDT dalla mamma?
243000090619           When  %found(FNLBL01L);
243100090619             if  not %open(FNBLP01L);
243200101213               open(e)  FNBLP01L;
243300101213               if  %error;
243400101213                 exsr  sr_ErrOpenFile;
243500101213               endif;
243600090619             endif;
243700090619             chain  (LBLaao : LBLlpo : LBLnro : LBLnso)  FNBLP000;
243800090619             if  %found(FNBLP01L);
243900090619               exsr  sr_DatiCLN_blp;
244000090619             endif;
244100090619
244200130716           // -?In Arrivo: reperimento dati DDT da FNARB?
244300090619           Other;
244400090619             exsr  sr_DatiCLN_arb;
244500090619
244600090619         EndSl;
244700090619
244800090619       ENDSR;
244900090619
245000090619       //--------------------------------------------------------------
245100090619       //?In Partenza: reperimento dati DDT da FNBLP                   ?
245200090619       //--------------------------------------------------------------
245300090619       BEGSR  sr_DatiCLN_blp;
245400090619
245500130716         // -?Numero DDT?
245600090619         select;
245700090619           when  BLPrmn <> *zero;
245800100906             if  BLPrmn  > -99999999   and
245900100906                 BLPrmn <   99999999;
246000100906               WddtCLN = BLPrmn;
246100100906             endif;
246200090619           when  BLPncd <> *zero;
246300090619             WddtCLN = BLPncd;
246400090619         endsl;
246500090619
246600130716         // -?Data DDT in formato gg/mm/aaaa?
246700090619         Data_Dmy = %date( ((BLPaas * 10000) + BLPmgs) : *iso );
246800100906         WdspCLN  = ( ( %subdt(Data_Dmy : *days)  * 10000 ) +
246900100906                      ( %subdt(Data_Dmy : *months) * 100 ) +
247000100906                ( %int( %subst( %editc(BLPaas : 'X') : 3 : 2 ) ) ) );
247100090619
247200090619       ENDSR;
247300090619
247400090619       //--------------------------------------------------------------
247500090619       //?In Arrivo: reperimento dati DDT da FNARB                     ?
247600090619       //--------------------------------------------------------------
247700090619       BEGSR  sr_DatiCLN_arb;
247800090619
247900130716         // -?Numero DDT?
248000090619         select;
248100090619           when  ARBrmn <> *zero;
248200100906             if  ARBrmn  > -99999999   and
248300100906                 ARBrmn <   99999999;
248400100906               WddtCLN = ARBrmn;
248500100906             endif;
248600090619           when  ARBncd <> *zero;
248700090619             WddtCLN = ARBncd;
248800090619         endsl;
248900090619
249000130716         // -?Data DDT in formato gg/mm/aaaa?
249100090619         Data_Dmy = %date( ((ARBaas * 10000) + ARBmgs) : *iso );
249200090619         WdspCLN  = ( ( %subdt(Data_Dmy : *days)  * 10000 ) +
249300090619                      ( %subdt(Data_Dmy : *months) * 100 ) +
249400110117                ( %int( %subst( %editc(ARBaas : 'X') : 3 : 2 ) ) ) );
249500090619
249600090619       ENDSR;
249700100517
249800100517       //--------------------------------------------------------------
249900100517       //?Verifica se presente ORM con preavviso mail                  ?
250000100517       //--------------------------------------------------------------
250100110117       BEGSR  sr_CheckORM_PreavvMail;
250200100517
250300130924         //clear  dOrm01;             ?(già fatto)?
250400100517
250500130925         // -?Reperimento num. ORM?
250600130924         exsr  RepAR4_M;
250700100517
250800130925         // -?SE ORM inesistente: inutile proseguire?
250900130925         if  §B4poe = *zero   or   §B4nor = *zero;
251000100517           leavesr;
251100100517         endif;
251200100517
251300130716         // -?Verifica presenza O.R.M.?
251400100517         if  Not  %open(FNORM01L);
251500101213           open(e)  FNORM01L;
251600101213           if  %error;
251700101213             exsr  sr_ErrOpenFile;
251800101213           endif;
251900100517         endif;
252000100517
252100100517         ORMpoe = §B4poe;
252200100517         ORMnsr = §B4nsr;
252300100517         ORMnor = §B4nor;
252400100517         ORMnrv = §B4nrv;
252500100517
252600100517         chain  K04ORM01  FNORM000;
252700100517
252800130716         // -?ORM non reperito: inutile proseguire?
252900100517         if  Not  %found(FNORM01L);
253000100517           leavesr;
253100100517         endif;
253200100517
253300130716         // -?Impostazione flag per stampa porto "ASSEGNATO"?
253400100517         dOrm01 = ORMflo;
253500100517         $PreavvORM = (§ORMpre = 'M');
253600100517
253700100517       ENDSR;
253800090619
253900080430       //--------------------------------------------------------------
254000140303       //?Preparazione dati di output relativi alla Lettera di Vettura ?
254100080430       //--------------------------------------------------------------
254200131016       BEGSR  sr_DatiOUT;
254300080430
254400101221         // -?Impostazione righe?
254500080430         clear LdV;
254600080430         clear wx;
254700080430
254800101221         // -?Area immagine LdV?
254900080801
255000110203         FOR  wx = 1  TO  c_Row;
255100080430
255200080430           %occur(LdV) = wx;
255300080430
255400080508           LdvPrg      = wx;
255500080430           select;
255600080430
255700101221             // ·? 1ª riga?
255800110203             when  wx = 1;
255900110203             LdvTxt = ds_01;
256000080430
256100101221             // ·? 2ª riga (vuota)?
256200110203             when  wx = 2;
256300080430             clear LdvTxt;
256400080430
256500101221             // ·? 3ª riga?
256600110203             when  wx = 3;
256700110203             LdvTxt = ds_03;
256800080430
256900101221             // ·? 4ª riga?
257000110203             when  wx = 4;
257100110203             LdvTxt = ds_04;
257200080430
257300101221             // ·? 5ª riga?
257400110203             when  wx = 5;
257500110203             LdvTxt = ds_05;
257600080430
257700101221             // ·? 6ª riga (vuota)?
257800110203             when  wx = 6;
257900110203             clear LdvTxt;
258000080430
258100101221             // ·? 7ª riga?
258200110203             when  wx = 7;
258300110203             LdvTxt = ds_07;
258400080430
258500101221             // ·? 8ª riga (vuota)?
258600110203             when  wx = 8;
258700080430             clear LdvTxt;
258800080430
258900101221             // ·? 9ª riga?
259000110203             when  wx = 9;
259100110203             LdvTxt = ds_09;
259200080430
259300101221             // ·?10ª riga?
259400110203             when  wx = 10;
259500110203             LdvTxt = ds_10;
259600080430
259700101221             // ·?11ª riga?
259800110203             when  wx = 11;
259900110203             LdvTxt = ds_11;
260000080430
260100101221             // ·?12ª riga?
260200110203             when  wx = 12;
260300110203             LdvTxt = ds_12;
260400080430
260500101221             // ·?13ª riga?
260600110203             when  wx = 13;
260700110203             LdvTxt = ds_13;
260800080430
260900101221             // ·?14ª riga?
261000110203             when  wx = 14;
261100110203             LdvTxt = ds_14;
261200080430
261300101221             // ·?15ª riga?
261400110203             when  wx = 15;
261500110203             LdvTxt = ds_15;
261600080430
261700101221             // ·?16ª riga?
261800110203             when  wx = 16;
261900110203             LdvTxt = ds_16;
262000080430
262100101221             // ·?17ª riga?
262200110203             when  wx = 17;
262300110203             LdvTxt = ds_17;
262400080430
262500101221             // ·?18ª riga?
262600110203             when  wx = 18;
262700110203             LdvTxt = ds_18;
262800080430
262900101221             // ·?19ª riga?
263000110203             when  wx = 19;
263100110203             LdvTxt = ds_19;
263200080430
263300101221             // ·?20ª riga?
263400110203             when  wx = 20;
263500110203             LdvTxt = ds_20;
263600080430
263700101221             // ·?21ª riga?
263800110203             when  wx = 21;
263900110203             LdvTxt = ds_21;
264000080430
264100101221             // ·?22ª riga?
264200110203             when  wx = 22;
264300110203             LdvTxt = ds_22;
264400080430
264500101221             // ·?23ª riga?
264600110203             when  wx = 23;
264700110203             LdvTxt = ds_23;
264800080430
264900101221             // ·?24ª riga?
265000110203             when  wx = 24;
265100110203             LdvTxt = ds_24;
265200080430
265300101221             // ·?25ª riga?
265400110203             when  wx = 25;
265500110203             LdvTxt = ds_25;
265600080430
265700101221             // ·?26ª riga (vuota)?
265800110203             when  wx = 26;
265900080806             clear LdvTxt;
266000080430
266100101221             // ·?27ª riga?
266200110203             when  wx = 27;
266300110203             LdvTxt = ds_27;
266400080430
266500101221             // ·?28ª riga?
266600110203             when  wx = 28;
266700110203             LdvTxt = ds_28;
266800081021
266900101221             // ·?29ª riga?
267000110203             when  wx = 29;
267100081021             LdvTxt = ds_29;
267200081021
267300101221             // ·?30ª riga?
267400110203             when  wx = 30;
267500081021             LdvTxt = ds_30;
267600081021
267700110215             // ·?31ª riga?
267800110203             when  wx = 31;
267900110215             LdvTxt = ds_31;
268000081021
268100101221             // ·?32ª riga?
268200110203             when  wx = 32;
268300081021             LdvTxt = ds_32;
268400081021
268500101221             // ·?33ª riga?
268600110203             when  wx = 33;
268700081021             LdvTxt = ds_33;
268800081021
268900101221             // ·?34ª riga?
269000110203             when  wx = 34;
269100081021             LdvTxt = ds_34;
269200081021
269300101221             // ·?35ª riga (vuota)?
269400110203             when  wx = 35;
269500081021             clear LdvTxt;
269600081021
269700101221             // ·?36ª riga (vuota)?
269800110203             when  wx = 36;
269900081021             clear LdvTxt;
270000080430
270100080430           endsl;
270200080430
270300080430         ENDFOR;
270400080430
270500110518         // -?Impostazione iniziale del modulo standard:?
270600131203         //// ·?Modulo standard con logo Bartolini?
270700131203         //TemplateName = 'ldvbase00 ';
270800131203         //// ·?Modulo standard con logo Bartolini-BRT?
270900131203         //TemplateName = 'ldvbase10 ';
271000120524         // ·?Modulo standard con logo BRT?
271100120524         TemplateName = 'ldvbase20 ';
271200080801
271300110203         Select;
271400081030
271500101221           // -?Area dati "DPD"?
271600101221           //  ?(NON della LNP, ma dalla fil. ricavata dal cod.mittente)?
271700110203           When  §CMntw = 'DPD';
271800110203             exsr  sr_LdVbaseDPD;
271900081030
272000101221           // -?Area dati "FedEx"?
272100150430           When  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
272200110203             exsr  sr_LdVbaseFED;
272300081030
272400131206           // -?Area box (ex "Promemoria per l'autotrasportatore")?
272500131206           When  $1o_Passaggio = *on  or  $ConsParz = *on;
272600110203             exsr  sr_LdVbase01;
272700081030
272800110203         EndSl;
272900130411
273000130411
273100130411         // ? CHIODI PER TEST                               ?
273200130411         Select;
273300130411           // -?Nessun TEST?
273400130411           When  $Test = *blank;
273500130411           // -?Spedizione FedEx?
273600130411           When  $Test = 'FED';
273700130411             TemplateName = 'ldvbase23 ';
273800130411           // -?Spedizione DPD?
273900130411           When  $Test = 'DPD';
274000130411             TemplateName = 'ldvbase22 ';
274100130411           // -?Spedizione con Consegna Parziale Precedente?
274200130411           When  $Test = 'C_P';
274300130411             TemplateName = 'ldvbase21 ';
274400130411           // -?Spedizione Normale?
274500130411           When  $Test = 'NOR';
274600130411             TemplateName = 'ldvbase20 ';
274700130411         EndSl;
274800130411
274900080801
275000101221         // -?Vuoti finali?
275100130213         For  wx = wx  To  w_Row;
275200080801           %occur(LdV) = wx;
275300130213           if  LdVPrg = *zero;
275400130213             LdvPrg = wx;
275500130213             clear  LdvTxt;
275600130213           endif;
275700110203         EndFor;
275800130205
275900130205         // -?Dati aggiuntivi per la 14ª riga (esenzione Iva)?
276000130205         if  DB0ift <> *zero  and
276100130205             DB0iva =  *zero  and
276200130205            (DB0cei <> *blank  or  DB0dce <> *blank);
276300130213           w_Row += 1;
276400130213           wx = w_Row;
276500130205           %occur(LdV) = wx;
276600130205           LdvPrg = 140;
276700130205           LdvTxt = ds_140;
276800130205         endif;
276900130205         // -?Dati aggiuntivi per la 18ª riga (n° fatt. e fil. Iva)?
277000130205         if  DB0nft <> *zero;
277100130213           w_Row += 1;
277200130213           wx = w_Row;
277300130205           %occur(LdV) = wx;
277400130205           LdvPrg = 180;
277500130205           LdvTxt = ds_180;
277600130205         endif;
277700110201
277800110201         // -?Codice a barre: ANNO+LNP+NRS+NSP+CKD:?
277900110201         //  ?richiama il pgm TRUL28R per ricavarne il check digit?
278000110201         reset  TRUL28ds;
278100110201         I28cod = BarCodeDS;
278200110201         TRUL28R1 ( TRUL28ds );
278300110201
278400110201         // -?Impostazione campo "LastRow"?
278500110201         LastRow = 'Spedizione: ' + %subst( O28cod : 1 : 15 ) + '  ' +
278600110203                   'Distinta: ' + %subst( ILVprmDst : 1 : 3 ) + '-'
278700120419                                + %subst( ILVprmDst : 4 );
278800080801
278900110203         // ______________________________
279000110203         //?Stampa di prova (SE richiesta)?
279100110203         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
279200110519         If  SDSprm > 11;
279300130716           exsr  sr_Print;
279400130716         EndIf;
279500080430
279600080430       ENDSR;
279700130716
279800130716       //--------------------------------------------------------------
279900130716       //?Stampa dei dati per l'immagine - SE RICHIAMATO PER PROVA     ?
280000130716       //--------------------------------------------------------------
280100130716       BEGSR  sr_Print;
280200130716
280300130716         // -?Apertura file di stampa?
280400130716         if not %open(QSYSPRT);
280500130716           open  QSYSPRT;
280600130716         endif;
280700130716
280800130716         // -?Stampa "testata"?
280900130716         except PRTtxt;
281000130716
281100130716         // -?Stampa dettaglio?
281200130716         for wx = 1 to w_Row;
281300130716           %occur(LdV) = wx;
281400130716           except PRTrow;
281500130716         endfor;
281600130716
281700130716         // -?Stampa "ultima riga"?
281800130716         except PRTend;
281900130716
282000130716         // -?Chiusura file di stampa?
282100130716         close QSYSPRT;
282200130716
282300130716       ENDSR;
282400081030
282500081030       //--------------------------------------------------------------
282600130716       //?Preparazione dati x spedizione "DPD"                         ?
282700081030       //--------------------------------------------------------------
282800130716       BEGSR  sr_LdVbaseDPD;
282900081030
283000120524         // -?Impostazione del modulo DPD:?
283100131203         //// ·?Modulo DPD con logo Bartolini?
283200131203         //TemplateName = 'ldvbase02 ';
283300131203         //// ·?Modulo DPD con logo Bartolini/BRT?
283400131203         //TemplateName = 'ldvbase12 ';
283500120524         // ·?Modulo DPD con logo BRT?
283600120524         TemplateName = 'ldvbase22 ';
283700081030
283800101221         // -?Impostazione data?
283900081030         %occur(LdV) = 19;
284000081030         %subst(LdVTxt : 94) = 'Date ' + PdspA;
284100081030
284200101221         // -?Impostazione numero colli?
284300081030         %occur(LdV) = 20;
284400081030         %subst(LdVTxt : 94) = 'Parcel N.' + PnclA;
284500081030
284600101221         // -?Impostazione numero parcel?
284700081030         %occur(LdV) = 21;
284800081030         %subst(LdVTxt : 94) = §B4Ipn;
284900081111
285000131015         // -?"Decodifica" del barcode?
285100081111         %occur(LdV) = 22;
285200081111         %subst(LdVTxt : 94) = *blank;
285300081111         if  §B4Ipn <> *blank;
285400101221           // -?controllo se vecchio o nuovo barcode?
285500081111           if  %subst(§B4Ipn : 14 : 1) = *blank;
285600081111             // - - vecchio
285700081111             %subst(LdVTxt : 94) = '01 ' + %subst(§B4Ipn : 1 : 3)
285800081111                                 + ' '   + %subst(§B4Ipn : 4 : 8);
285900081111           else;
286000081111             // - - nuovo
286100081111             %subst(LdVTxT : 94) = '01 ' + %subst(§B4Ipn : 1 :  4)
286200081111                                 + ' '   + %subst(§B4Ipn : 5 : 10);
286300081111           endif;
286400081111         endif;
286500081111
286600130716         // -?Impostazione costanti  (LB ha detto di NON metterle)?
286700081111         //%occur(LdV) = 23;
286800081111         //%subst(LdVTxt : 94) = *blank;
286900081111         //%occur(LdV) = 24;
287000081111         //%subst(LdVTxt : 94) = 'Controllare la corrispon-';
287100081111         //%occur(LdV) = 25;
287200081111         //%subst(LdVTxt : 94) = 'denza del destinatario con';
287300081111         //%occur(LdV) = 26;
287400081111         //%subst(LdVTxt : 94) = 'l''indirizzo dell''etichetta';
287500081111         //%occur(LdV) = 27;
287600081111         //%subst(LdVTxt : 94) = 'DPD.';
287700081030
287800081030       ENDSR;
287900081030
288000081030       //--------------------------------------------------------------
288100130716       //?Preparazione dati x spedizione "FedEx"                       ?
288200081030       //--------------------------------------------------------------
288300130716       BEGSR  sr_LdVbaseFED;
288400081030
288500120524         // -?Impostazione del modulo FedEx:?
288600131203         //// ·?Modulo FedEx con logo Bartolini?
288700131203         //TemplateName = 'ldvbase03 ';
288800131203         //// ·?Modulo FedEx con logo Bartolini/BRT?
288900131203         //TemplateName = 'ldvbase13 ';
289000120524         // ·?Modulo FedEx con logo BRT?
289100120524         TemplateName = 'ldvbase23 ';
289200110517
289300081103         w_Row  = c_Row_B;
289400081030         Wtrc = 'C';
289500081030
289600101221         // -?Preparazione elenco segnacolli FedEx?
289700101213         if  NOT %open(FNART01L);
289800101213           open(e)  FNART01L;
289900101213           if  %error;
290000101213             exsr  sr_ErrOpenFile;
290100101213           endif;
290200101213         endif;
290300101213         if  NOT %open(FIARS01L);
290400101213           open(e)  FIARS01L;
290500101213           if  %error;
290600101213             exsr  sr_ErrOpenFile;
290700101213           endif;
290800101213         endif;
290900081030
291000101221         // -?Impostazione numeratore fino alla 39ª riga?
291100101213         FOR  wx = wx  TO (w_Row - c_Row_Fed);
291200081030
291300081030           %occur(LdV) = wx;
291400081030           LdvPrg      = wx;
291500081030
291600101213         ENDFOR;
291700081030
291800101221         // -?40ª riga & successive?
291900081030         clear  xx;
292000081030         clear  Wnsc;
292100101221         FOR  wx = (w_Row - c_Row_Fed + 1)  TO w_Row;
292200081030
292300081030           %occur(LdV) = wx;
292400081030           LdvPrg      = wx;
292500081030
292600081030           // - Caricamento AWB FedEx
292700081103           clear  wAWB;
292800101221           // - -?leggo segnacolli per bolle in arrivo?
292900081030           exsr  CarAWB_Arx;
293000081030           if  wAWB = *blank;
293100081030             leave;
293200081030           endif;
293300081030
293400081030           xx    += 1;
293500081103           Pptn   = wAWB;
293600081030           Psts   = %editc( xx : 'Z' );
293700081030           LdvTxt = ds_Fed;
293800081030
293900101221         ENDFOR;
294000081030
294100081030       ENDSR;
294200081030
294300081030       //--------------------------------------------------------------
294400130716       //?Riempimento del box "Promemoria per l'autotrasportore"       ?
294500081030       //--------------------------------------------------------------
294600130716       BEGSR  sr_LdVbase01;
294700081030
294800120524         // -?Impostazione del modulo con Box aggiuntivo (14 rec. in +)?
294900131203         //// ·?Modulo Box aggiuntivo con logo Bartolini?
295000131203         //TemplateName = 'ldvbase01 ';
295100131203         //// ·?Modulo Box aggiuntivo con logo Bartolini/BRT?
295200131203         //TemplateName = 'ldvbase11 ';
295300120524         // ·?Modulo Box aggiuntivo con logo BRT?
295400120524         TemplateName = 'ldvbase21 ';
295500110517
295600081103         w_Row  = c_Row_B;
295700081030
295800101221         // -?Intestazione box "Promemoria per l'autotrasportatore"?
295900131206         //  ?(NON LA SI FA PIÙ)?
296000131206         //exsr  sr_IntestBox;
296100131206
296200131206
296300131206         // -?Posizionamento sulla 36ª riga?
296400131206         //  ?(Il ciclo "FOR" precedente dovrebbe aver già posizionato?
296500131206         //  ? sulla 37ª riga)?
296600131206         wx = 36;
296700131206
296800131206         // -?Se reperito il 1° PASSAGGIO (c/o destinatario):?
296900131206         IF  $1o_Passaggio;
297000131206
297100131206           test(de) EVBdev;
297200131206           if %error;
297300131206             clear w0080;
297400131206           else;
297500131206             Data_Eur = %date( %char(EVBdev) : *iso0 );
297600131206             w0080    = %dec(Data_Eur);
297700131206           endif;
297800131206
297900131206           // -?37ª riga?
298000131206           wx += 1;
298100131206           %occur(LdV) = wx;
298200131206           LdvPrg      = wx;
298300131206           Ppma    = 'Primo passaggio del '
298400131206                   + %editc( w0080 : 'Y' )
298500131206                   + ' effettuato alle '
298600131206                   + %editw( EVBhev: '  :  ' );
298700131206           LdvTxt  = ds_37;
298800131206
298900131206           // -?38ª riga: VUOTA (separatrice)?
299000131206           wx += 1;
299100131206           %occur(LdV) = wx;
299200131206           LdvPrg      = wx;
299300131206           clear  LdvTxt;
299400131206
299500131206         ENDIF;
299600131206
299700081030
299800131206         // -?Se reperite CONSEGNE PARZIALI PRECEDENTI:?
299900131206         IF  $ConsParz = *on;
300000131206
300100131206           // -?Impostazione segnalazione in output?
300200131206           xx = 1;
300300131206           DoW  (xx <= %elem($CP)   and   $CP(xx) < *all'9');
300400131206
300500131206             Elem_CP = $CP(xx);
300600131206             test(de) WCPdcm;
300700131206             if %error;
300800131206               clear w0080;
300900131206             else;
301000131206               Data_Eur = %date( %char(WCPdcm) : *iso0 );
301100131206               w0080    = %dec(Data_Eur);
301200131206             endif;
301300131206
301400131206             // -?37ª o 39ª riga & successive?
301500131206             wx += 1;
301600131206             %occur(LdV) = wx;
301700131206             LdvPrg      = wx;
301800131206             Ppma    = 'Consegnati colli '
301900131206                     + %editc(WCPncl:'Z')
302000131206                     + ' in data '
302100140304                     + %editc(w0080:'Y');
302200140304             if  WCPocm > *zero;
302300140304               Ppma    = %trimr( Ppma )
302400140304                       + ' alle ore '
302500140304                       + %editw( WCPocm : '0 :  ' );
302600140304             endif;
302700140304             if  WCPfir <> *blank;
302800140304               Ppma    = %trimr( Ppma )
302900140303                     + ' - Firmatario: '
303000140303                     + WCPfir;
303100140304             endif;
303200131206             LdvTxt  = ds_37;
303300131206
303400131206             xx += 1;
303500131206
303600131206           EndDo;
303700131206
303800131206         ENDIF;
303900081030
304000081030       ENDSR;
304100080801
304200080801       //--------------------------------------------------------------
304300130716       //?Intestazione del box "Promemoria per l'autotrasportore"      ?
304400080801       //--------------------------------------------------------------
304500131206       //BEGSR  sr_IntestBox;
304600131206       //
304700131206       //  // 37ª riga
304800131206       //  //wx += 1;         (già incrementato dal ciclo "FOR" precedente)?
304900131206       //  %occur(LdV) = wx;
305000131206       //  LdvPrg      = wx;
305100131206       //  Ppma    = '                             '
305200131206       //          + c_PMxA;
305300131206       //  LdvTxt  = ds_37;
305400131206       //
305500131206       //  // -?38ª riga?
305600131206       //  wx += 1;
305700131206       //  %occur(LdV) = wx;
305800131206       //  LdvPrg      = wx;
305900131206       //  Ppma    = *all'=';
306000131206       //  LdvTxt  = ds_37;
306100131206       //
306200131206       //ENDSR;
306300101221
306400080430      /end-free
306500081030      *
306600081030      *---------------------------------------------------------------*
306700081030      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
306800081030      *---------------------------------------------------------------*
306900081030     c     CarAWB_Arx    BEGSR
307000130215      *
307100130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
307200130215      *   comunque una bolla mamma
307300130215     c                   exsr      sr_Mamma_FedEx
307400081030      *
307500081030      * Posizionamento
307600130215if  2c                   if        $Mamma = *off   and
307700130215     c                             $Mamma_FedEx = *off
307800081030     c     KLDVnsc       setgt     FNART000
307900081030x   2c                   else
308000081030     c     KLBLnsc       setgt     FNART000
308100081030e   2c                   endif
308200081030      *
308300081030      * Ciclo di lettura
308400081030do  1c                   DO        *hival
308500081030      *
308600130215if  2c                   if        $Mamma = *off   and
308700130215     c                             $Mamma_FedEx = *off
308800081030     c     KLDV          reade     FNART000
308900081030x   2c                   else
309000081030     c     KLBL          reade     FNART000
309100081030e   2c                   endif
309200081030if  2c                   if        %eof(FNART01L)
309300081030     c                   leave
309400081030e   2c                   endif
309500081030      * - per ogni segnacollo cerco l'AWB
309600081030     c     K05ARS01A     chain     FIARS000
309700081030if  2c                   if            %found(FIARS01L)
309800081030     c                             and %subst(ARSnot:1:1) >= *zeros
309900081030     c                   eval      wAWB = %subst(ARSnot:1:12)
310000081030     c                   eval      wNSC = ARSnsc
310100081030     c                   leave
310200081030e   2c                   endif
310300081030      *
310400081030e   1c                   ENDDO
310500081030      *
310600081030     c                   ENDSR
310700130215      *
310800130215      /free
310900130215
311000130215       //--------------------------------------------------------------
311100130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
311200130215       //--------------------------------------------------------------
311300130215       BEGSR  sr_Mamma_FedEx;
311400130215
311500130924         //$Mamma_FedEx = *off;         ?(già così)?
311600130215
311700130924         If  Not $Mamma;
311800130215
311900130215           // -?Verifica se la spedizione ha comunque mamma?
312000130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
312100130215           if  not %open(FNLBL01L);
312200130215             open  FNLBL01L;
312300130215           endif;
312400130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
312500130215
312600130215           $Mamma_FedEx = %found(FNLBL01L);
312700130215
312800130215         EndIf;
312900130215
313000130215       ENDSR;
313100130215
313200130215      /end-free
313300131008      *
313400131008      *---------------------------------------------------------------*
313500131008      *?Prepara elenco SegnaColli Non Sequenziali (dett.) - MAX 2 -  ?*
313600131008      *?  da FNART (arrivo)                                          ?*
313700131008      *---------------------------------------------------------------*
313800131008     c     ListaScNSd    BEGSR
313900131008      *
314000131008      * SE ci sono solo 2 segnacolli non sequenziali:
314100131008      *   staranno al posto del range - basta non indicare
314200131008      *   "NON SEQUENZIALI"
314300131008      *
314400131008if  1c                   if        not %open(FNART01L)
314500131008     c                   open(e)   FNART01L
314600131008      /free
314700131008           if  %error;
314800131008             exsr  sr_ErrOpenFile;
314900131008           endif;
315000131008      /end-free
315100131008e   1c                   endif
315200131008      *
315300131008      * memorizzo l'ultimo numero di segnacollo
315400131008if  1c                   if        $Mamma = *off
315500131008     c     KLDV          setgt     FNART000
315600131008     c     KLDV          readpe    FNART000
315700131008x   1c                   else
315800131008     c     KLBL          setgt     FNART000
315900131008     c     KLBL          readpe    FNART000
316000131008e   1c                   endif
316100131008if  1c                   if        NOT %eof(FNART01L)
316200131008     c                   eval      %subst(Pnsx : 8) = ' , '
316300131008     c                                              + %editc(ARTnsc : 'Z')
316400131008e   1c                   endif
316500131008      *
316600131008      * memorizzo il primo numero di segnacollo
316700131008if  1c                   if        $Mamma = *off
316800131008     c     KLDV          setll     FNART000
316900131008     c     KLDV          reade     FNART000
317000131008x   1c                   else
317100131008     c     KLBL          setll     FNART000
317200131008     c     KLBL          reade     FNART000
317300131008e   1c                   endif
317400131008if  1c                   if        NOT %eof(FNART01L)
317500131008     c                   eval      %subst(Pnsx : 1 : 7) =
317600131008     c                                 %editc(ARTnsc : 'Z')
317700131008e   1c                   endif
317800131008      *
317900131008     c                   ENDSR
318000030515      *
318100030515      *---------------------------------------------------------------*
318200050217      *?Carico LDV da FNARB00F                                       ?*
318300030515      *---------------------------------------------------------------*
318400110208     c     sr_CarARB     BEGSR
318500030515      *
318600110202      /free
318700110202
318800110202         //____________________________________________________________
318900110202         //? RISULTATO è impostato di default a "OK" => creo immagine ?
319000110202         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
319100110202
319200110202         // »? STEP 1 ?«
319300131203         //  ?Verifica se presenti forzature per la distinta           ?
319400131203         //  ?(vedi file FIIRD00F)                                     ?
319500131203
319600131203         //?-?Verifica se distinta in FIIRD10F                           ?
319700131203         //  ?e se elaborabile con (IRDTPELAB = *blank) o senza controlli?
319800130716         chain  %kds(k01fiird11)  FIIRD000;
319900130313
320000130716         If  %found(FIIRD11L)  and  IRDtpElab = *blank;
320100131203           // -?SE rileva errori =>?
320200131203           // -?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
320300130716           exsr  sr_STEP_1_CtrlTASFLO;
320400130716         EndIf;
320500131203
320600131203         // »? STEP 2 ?«
320700131203         //  ?Verifica se spedizione elaborabile                       ?
320800131203         //  ?(controlli su distinta e spedizione nel file FIILV00F)   ?
320900131203         //  ?- SE distinta SENZA forzature in FIIRD00F -              ?
321000110202
321100110204         // -?Reperimento dati distinta (FIDST)?
321200110228         clear  dDstflr;
321300110202         k_DSTnpg = 4;
321400110202         k_DSTnfv = ds_Distinta.p_Ndc;
321500110202         k_DSTfgs = ds_Distinta.p_Fgs;
321600110202         chain  %kds(k03fidst01)  FIDST000;
321700110228         if  %found(FIDST01L);
321800110228           dDSTflr = DSTflr;
321900110228         endif;
322000110202
322100110202         // -?Controllo distinta:?
322200130920         //?·?(SE Distinta NON trovata in FIIRD10F per recupero immagini?
322300130924         //  ?  & NON è una spedizione FORZATA PER TEST)                ?
322400131016         //  ?SE distinta NON trovata in FIDST01L: registra LOG e prosegue?
322500131016         //  ?SE distinta NON chiusa:?
322600131016         //  ?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
322700130716         If  Not %found(FIIRD11L)  and
322800130716             $Test = *blank;
322900131203           exsr  sr_STEP_2_CtrlDISTINTA;
323000130716         EndIf;
323100110202
323200110202
323300110202         // -?Reperimento dati Immagine LdV (FIILV)?
323400110208         clear  ILVcol;
323500110309         clear  wCountRec01;
323600110202         chain(e)  %kds(k06fiilv01)  FIILV000;
323700130313
323800130716         //?·?DISTINTA in FIIRD10F => recupero immagine (senza ctrl)?
323900131016         //?·?DISTINTA NON in FIIRD10F => controlli da eseguire     ?
324000131016         //  ?SE trovati errori:?
324100131016         //  ?"risponde" ed eventualmente esce dalla s.p. (direttamente dalla subr.)?
324200130716         If  Not  %found(FIIRD11L);
324300131203           exsr  sr_STEP_2_CtrlFIILV;
324400130716         EndIf;
324500130313
324600131203         // »? STEP 3 ?«
324700131203         //  ?Controllo spedizione                                     ?
324800131203         //  ?(da eseguire comunque)                                   ?
324900110202
325000131203         // -?Reperimento dati Spedizione (FNARB)?
325100110207         chain  KLDV  FNARB000;
325200101220
325300110202         // -?Spedizione non trovata?
325400101221         if  NOT %found(FNARB01L);
325500130924           // ·?Spedizione non trovata => ERRORE: non rielaborare immagine?
325600131203           //  ?"risponde" ed esce dalla s.p.?
325700110204           Risultato = 'XX';
325800130924           //RtnOpCode = 'FNARB_MANC';
325900130924           //RtnEsito  = -1;
326000130924           // -?Registrazione log in FIILV, differenziando l'errore per?
326100130924           //  ?iter normale (25) o iter con forzatura (52)?
326200130924           if  Not  %found(FIIRD11L);
326300130313             wCod_Log = '25';
326400130716           else;
326500130716             wCod_Log = '52';
326600130716           endif;
326700110208           exsr  sr_WrtFIILV;
326800101221           // -?Uscita dalla s.p.?
326900110204           exsr  sr_End;
327000101221         endif;
327100101221
327200101221
327300110202         // -?Registrazione record per spedizione "OK" (root "buono")?
327400110218         If  wCod_Log = *blank;
327500130206           RootFolder = §ILVimgOk;
327600110203           wCod_Log = '01';
327700110202           exsr  sr_WrtFIILV;
327800110218         EndIf;
327900090121
328000080523      /end-free
328100070514      *
328200070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
328300070514      *   il reperimento dell'eventuale bolla mamma...
328400070514     c                   eval      wB0ntc =  ARBntc
328500120524     c                   eval      wB0dcp =  ARBdcp
328600120524     c                   eval      wB0tmc =  ARBtmc
328700120524     c                   move      ARBpdc        Wpdc4a
328800080523     c                   eval      wB0dcm =  ARBdcm
328900121018     c                   eval      wB0piv =  ARBcpi
329000030515      *
329100030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
329200030515     c                   exsr      sr_DS_ARB
329300030515      *
329400070308      * Decodifico filiale di Arrivo
329500131203     c                   exsr      sr_DecLNA
329600030515      *
329700030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
329800030515      *    "generiche" (3A, TB, 5E)
329900030515     c                   exsr      sr_DS_TAB
330000030515      *
330100030515      * Ultimi dati mancanti:
330200030515      * - Cod. cliente mittente
330300030515if  1c                   If        ARBccm <> *zeros
330400030515     c                   movel     ARBccm        DB0ccm
330500030515x   1c                   Else
330600030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
330700030515     c                   movel     ARBksc        DB0ccm
330800030515e   2c                   endif
330900030515e   1c                   EndIf
331000030515      * - Cod. cliente destinatario
331100030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
331200030515     c                   movel     ARBksc        DB0ccd
331300030515e   1c                   endif
331400030515      * - Se c'è 2ª bolla imposto LNA9999
331500030515if  1c                   if        DB0tb2 <> *blanks
331600030515     c                   eval      DB0ccd = *all'9'
331700030515     c                   movel     ARBlna        DB0ccd
331800030515e   1c                   endif
331900120524      * - Reperimento Data spedizione
332000120524if  1c                   if        DB0mgs <> *zeros
332100120524     c                   z-add     DB0mgs        Wdsp
332200120524     c                   movel     DB0aas        Wdsp
332300120524x   1c                   else
332400120524      *   prendo la data del giorno se non c'è la data spedizione
332500120524     c                   time                    w0140
332600120524     c                   move      w0140         w0080
332700120524     c     *eur          movel     w0080         Data_EUR
332800120524     c     *iso          movel     Data_EUR      Wdsp
332900120524e   1c                   endif
333000030515      *
333100061030      * 3ª impostaz. campi della DS: importi da calcolare
333200061031     c                   exsr      sr_DS_IMPO
333300030515      *
333400080505     c                   ENDSR
333500080724      /free
333600130716
333700130716       //--------------------------------------------------------------
333800130716       //?»  STEP 1  «                                                 ?
333900130716       //?Distinta in FIIRD10F e senza forzature (IRDTPLAB = *blank):  ?
334000130716       //?verifica se immagine già generata (DTASFLO.§FLOIML <> *blank)?
334100130716       //--------------------------------------------------------------
334200130716       BEGSR  sr_STEP_1_CtrlTASFLO;
334300130716
334400130716         // -?Reperimento spedizione da TITAS per verificarne lo stato?
334500131016         //  ?della relativa immagine.  Se già presente immagine:?
334600131016         //  ?"risponde" con codice errore 51 e "non elaborare"?
334700131016         //  ?ed esce dalla s.p.?
334800130716         if  %subst( wSysName : 1 : 6 ) = 'SETRAS';
334900130716           wLibFileTITAS = 'GAITRAGRU/TITAS30C';
335000130716         else;
335100130716           wLibFileTITAS = 'GAITRAGRPS/TITAS30C';
335200130716         endif;
335300130716         if  NOT  %open(TITAS30C);
335400130716           open(e)  TITAS30C;
335500130716           if  %error;
335600130716             exsr  sr_ErrOpenFile;
335700130716           endif;
335800130716         endif;
335900130716
336000130716         chain  %kds(k05titas30 : 4)  TITAS30C;
336100130716
336200130716         If  %found(TITAS30C)            and
336300130716             TASdcm > *zero              and
336400130716             TASndc = ds_Distinta.p_Ndc  and
336500130716             %int( TASpdc / 10000 ) = ds_Distinta.p_Fgs;
336600130716
336700130716           dTASflo = TASflo;
336800130716
336900130716           if  dTASflo.§FLOiml <> *blank;
337000130716             Risultato = 'XX';
337100130924             //RtnOpCode = 'GIÀ_IMMAG ';
337200130924             //RtnEsito  = -1;
337300130716             // -?Registrazione log in FIILV?
337400130716             wCod_Log = '51';
337500130716             exsr  sr_WrtFIILV;
337600130716             // -?Uscita dalla s.p.?
337700130716             exsr  sr_End;
337800130716           endif;
337900130716
338000130716         EndIf;
338100130716
338200130716       ENDSR;
338300130716
338400130716       //--------------------------------------------------------------
338500131203       //?»  STEP 2  «                                                 ?
338600131203       //?Distinta NON in FIIRD10F:?controlli sulla distinta.          ?
338700130716       //--------------------------------------------------------------
338800131203       BEGSR  sr_STEP_2_CtrlDISTINTA;
338900130716
339000130716         Select;
339100130716
339200130716           // ·?DISTINTA non trovata => ANOMALIA?
339300130716           When  NOT  %found(FIDST01L)  or
339400130716                 DSTatb <> *blank;
339500130924             // -?Registrazione log in FIILV - se non già fatto?
339600130716             if  wCod_Log = *blank;
339700130716               wCod_Log = '22';
339800130716               exsr  sr_WrtFIILV;
339900130716             endif;
340000130716
340100131016           // ·?DISTINTA non chiusa => ripetere la richiesta?
340200130716           When  DSTfcf = *blank;
340300130716             Risultato  = 'NO';
340400130924             //RtnOpCode  = 'DIST_APERT';
340500130716             //RtnEsito   = -1;
340600130716             // -?Uscita dalla s.p.?
340700130716             exsr  sr_End;
340800130716
340900130716         EndSl;
341000130716
341100130716       ENDSR;
341200130716
341300130716       //--------------------------------------------------------------
341400131203       //?»  STEP 2  «                                                 ?
341500131203       //?Distinta NON in FIIRD10F:?controlli sul/i record in FIILV00F.?
341600130716       //--------------------------------------------------------------
341700131203       BEGSR  sr_STEP_2_CtrlFIILV;
341800130716
341900130716         Select;
342000130716
342100130716           // ·?Rec. IMMAGINE LdV allocato => ripetere la richiesta?
342200130716           When  %error;
342300130716             Risultato  = 'NO';
342400130924             //RtnOpCode  = 'FIILV_ALLC';
342500130716             //RtnEsito   = -1;
342600130716             // -?Uscita dalla s.p.?
342700130716             exsr  sr_End;
342800130716
342900130716           // ·?Rec. IMMAGINE LdV non trovato?
343000130716           When  NOT  %found(FIILV01L);
343100130716             select;
343200130716               // ·?DISTINTA senza scarico a PDA delle spedizioni?
343300130716               //  ?=> cancellare immagine (senza LOG)?
343400130716               when  DSTpda <> 'C'   and   DSTpda <> 'E';
343500130716                 Risultato  = 'XX';
343600130924                 //RtnOpCode  = 'DIST_NOPDA';
343700130716                 //RtnEsito   = -1;
343800130716                 // -?Uscita dalla s.p.?
343900130716                 exsr  sr_End;
344000130716               // ·?DISTINTA in test consegne?
344100130716               //  ?=> cancellare immagine (senza LOG)?
344200130716               when  §DSTtstPda = 'C'   or   §DSTtstPda = 'E';
344300130716                 Risultato  = 'XX';
344400130924                 //RtnOpCode  = 'DIST_TEST ';
344500130716                 //RtnEsito   = -1;
344600130716                 // -?Uscita dalla s.p.?
344700130716                 exsr  sr_End;
344800130716               // ·?Altrimenti: ANOMALIA?
344900130924               //  ?Registrazione log in FIILV - se non già fatto?
345000130716               other;
345100130716                 if  wCod_Log = *blank;
345200130716                   wCod_Log = '23';
345300130716                   exsr  sr_WrtFIILV;
345400130716                 endif;
345500130716             endsl;
345600130716
345700130716           // ·?Rec. IMMAGINE LdV già elaborato?
345800130716           //  ?=> ricerca record NON elaborato (ILVCOL = "00")?
345900130716           When  ILVcol <> '00';
346000130716             exsr  sr_CercaLog00;
346100130716             // ·?Rec. IMMAGINE LdV non trovato (né "00" né "01")?
346200130924             //  ?Registrazione log in FIILV - se non già fatto?
346300130716             if  ILVcol <> '00'  and  wCountRec01 = *zero
346400130716                                 and  wCod_Log    = *blank;
346500130716               wCod_Log = '23';
346600130716               exsr  sr_WrtFIILV;
346700130716             endif;
346800130716
346900130716         EndSl;
347000130716
347100130716
347200130716         // -?Verifica SE trovato record da elaborare per la sped/dist?
347300130716         $FIILV = (ILVcol = '00');
347400130716
347500130716         // -?Impostazione struttura dati del campo ILVLIBERO?
347600130716         dILVlibero = ILVlibero;
347700130716
347800130716
347900130716         Select;
348000130716
348100130716           // -?Spedizione non consegnata in distinta?
348200130716           When  ILVnoCDst = 'S';
348300130924             // -?Registrazione log in FIILV - se non già fatto?
348400130716             if  wCod_Log = *blank;
348500130716               wCod_Log = '08';
348600130716               exsr  sr_WrtFIILV;
348700130716             endif;
348800130716
348900130716           // -?LdV da scannerizzare?
349000130716           When  ILVscan = *blank;
349100130924             // -?Registrazione log in FIILV - se non già fatto?
349200130716             if  wCod_Log = *blank;
349300130716               wCod_Log = '09';
349400130716               exsr  sr_WrtFIILV;
349500130716             endif;
349600130716
349700130716         EndSl;
349800130716
349900130716       ENDSR;
350000130716
350100130716       //--------------------------------------------------------------
350200130716       //?Ricerca record IMMAGINE LDV NON elaborato (ILVCOL = "00")    ?
350300130716       //--------------------------------------------------------------
350400130716       BEGSR  sr_CercaLog00;
350500130716
350600130716         //?N.B.?Il 1° rec. per spedizione/distinta è già stato letto?
350700130716
350800130716         DoU  ILVcol = '00';
350900130716
351000130716           // ·?Rec. Immagine LdV già elaborato ("01")?
351100130716           if  ILVcol = '01';
351200130716             wCountRec01 += 1;
351300130716           endif;
351400130716
351500130716           reade(e)  %kds(k06fiilv01)  FIILV000;
351600130716
351700130716           select;
351800130716
351900130716             // ·?Rec. Immagine LdV allocato => ripetere la richiesta?
352000130716             when  %error;
352100130716               Risultato  = 'NO';
352200130924               //RtnOpCode  = 'FIILV_ALLC';
352300130716               //RtnEsito   = -1;
352400130716               // -?Uscita dalla s.p.?
352500130716               exsr  sr_End;
352600130716
352700130716             // ·?Solo rec. Immagine LdV già elaborati?
352800130716             //  ?=> scrittura nuovo?
352900130716             when  %eof(FIILV01L);
353000130716               clear  ILVcol;
353100130716               leave;
353200130716
353300130716             // ·?Già reperiti 100 rec. per le stesse sped./dist.?
353400130716             //  ?(questo è in realtà il 101°)?
353500131016             //  ?=> NON ripetere la richiesta (senza LOG)?
353600130716             when  wCountRec01 >= 100;
353700130716               Risultato  = 'XX';
353800130924               //RtnOpCode  = 'SPEDGIÀ100';
353900130716               //RtnEsito   = -1;
354000130716               // -?Invio e-mail?
354100130716               exsr  sr_SendMAIL;
354200130716               // -?Uscita dalla s.p.?
354300130716               exsr  sr_End;
354400130716
354500130716             // ·?Rec. Immagine LdV già elaborato con data/ora?
354600130716             //  ?creazione immagine sul PDA successive all'attuale?
354700131016             //  ?=> NON ripetere la richiesta (senza LOG)?
354800130716             //when  DataOraCrea <= ILVprmTimP;
354900130716             when  DataOraCrea < ILVprmTimP;
355000130716               Risultato  = 'XX';
355100130924               //RtnOpCode  = 'SPED_GIÀEL';
355200130716               //RtnEsito   = -1;
355300130716               // -?Uscita dalla s.p.?
355400130716               exsr  sr_End;
355500130716
355600130716             // ·?Rec. Immagine LdV già elaborato con data/ora?
355700130716             //  ?creazione immagine sul PDA identica all'attuale?
355800130716             //  ?=> riprovare dopo 10 minuti (senza LOG)?
355900130716             when  DataOraCrea = ILVprmTimP  and
356000130716                   ILVtimA > %int( %subst( %char( %dec(
356100130716                                   %timestamp() - %minutes(10) ) )
356200130716                                           : 1 : 14 ) );
356300130716               Risultato  = 'NO';
356400130924               //RtnOpCode  = '..TRA10MIN';
356500130716               //RtnEsito   = -1;
356600130716               // -?Uscita dalla s.p.?
356700130716               exsr  sr_End;
356800130716
356900130716           endsl;
357000130716
357100130716         EndDo;
357200130716
357300130716       ENDSR;
357400101221
357500080724       //--------------------------------------------------------------
357600130716       //?Registrazione record nel work-file FIILV00F                  ?
357700080724       //--------------------------------------------------------------
357800110207       BEGSR  sr_WrtFIILV;
357900110202
358000110202         // -?Segnalazione anomalia rilevata per la spedizione?
358100110204         if  Not  $FIILV;
358200110202           clear  FIILV000;
358300110202           ILVfgs = ds_Distinta.p_Fgs;
358400110202           ILVndc = ds_Distinta.p_Ndc;
358500110202           ILVaas = DB0aas;
358600110202           ILVlnp = DB0lnp;
358700110202           ILVnrs = DB0nrs;
358800110202           ILVnsp = DB0nsp;
358900110202         endif;
359000110202
359100110207         ILVcol     = wCod_Log;
359200110202         xx = %lookup( wCod_Log : $CoL );
359300110202         if  xx > *zero;
359400110202           ILVnot   = $DeL(xx);
359500110202         endif;
359600110202         ILVprmSpe  = Spedizione;
359700110202         ILVprmDst  = Distinta;
359800110202         ILVprmTimp = DataOraCrea;
359900110202         ILVrisult  = Risultato;
360000110202         ILVtimA    = %int( %subst( %char( %dec( %timestamp() ) ) :1 :14 ) );
360100130411
360200130411
360300131203         // -? Spedizioni FORZATE PER TEST => NON scrive il rec.       ?
360400130411         IF  $Test <> *blank;
360500130411           leavesr;
360600130411         ENDIF;
360700130411
360800090210
360900110923
361000131203         // -?S.P. richiamata per stampa cartacea => NON scrive il rec.?
361100110923         If  SDSprm > 11;
361200110923           leavesr;
361300110923         EndIf;
361400110923
361500110923
361600110204         if  $FIILV;
361700110202           //_______________
361800110202           update  FIILV000;
361900110202           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
362000110202         else;
362100110204           //_______________
362200110204           write   FIILV000;
362300110204           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
362400110202         endif;
362500080724
362600080724       ENDSR;
362700130227
362800130227       //--------------------------------------------------------------
362900130716       //?Invio e-mail per avvisare degli oltre 100 rec. con cod. LOG  ?
363000130716       //?   "01" per la stessa spedizione in FIILV.                   ?
363100130227       //--------------------------------------------------------------
363200130227       BEGSR  sr_SendMAIL;
363300130227
363400130227         // -?Reperimento tab. "MRA"?
363500130227         reset  dMRAdan;
363600130227         reset  TIBS02ds;
363700130227         T02cod = 'MRA';
363800130227         T02ke1 = SDSpgm;
363900130227         T02ke2 = *blank;
364000130227         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
364100130227         if  T02err = *blank;
364200130227           dMRAdan = T02uni;
364300130227         endif;
364400130227
364500130227         // -?Override ai file di stampa?
364600130227         //  ?(per impostarvi i dati per l'invio via e-mail)?
364700130227         exsr sr_OvrPrtF;
364800130227
364900130227         // -?Stampa segnalazione?
365000130227         except  EmlTxt;
365100130227         P_Fgs2 = ds_Distinta.p_Fgs;
365200130227         P_Ndc2 = ds_Distinta.p_Ndc;
365300130227         except  EmlDet;
365400130227         except  EmlEnd;
365500130227
365600130227         // -?Cancellazione Override ai file di stampa?
365700130227         exsr sr_DeleteOvr;
365800130227
365900130227       ENDSR;
366000130227
366100130227       //--------------------------------------------------------------
366200130716       //?Override al file di stampa per impostarvi i dati per         ?
366300130716       //?  l'invio via e-mail.                                        ?
366400130227       //--------------------------------------------------------------
366500130227       BEGSR  sr_OvrPrtF;
366600130227
366700130227         reset  TRTCM1ds;
366800130227         if  §MRAdmitt <> *blank;
366900130227           §CM1mitt = %trim(§MRAdmitt);
367000130227         endif;
367100130227         §CM1dst  = %trim(§MRAddest);
367200130227         §CM1tips = §MRAdreg;
367300130227         //§CM1var  = '*OBJM*' + §MRAddes;
367400130227         §CM1idp  = §MRAdidpro;
367500130227
367600130227         // -?Override al "corpo e-mail"?
367700130227         if  §MRAdreg <> *blank;
367800130227           Qcmd = c_CmdOvrPrtF
367900130227                + ' outq(' + %trim(§MRAdoutqi) + ')'
368000130227                + ' usrdfndta(''' + TRTCM1ds + ''')';
368100130227         else;
368200130227           Qcmd = c_CmdOvrPrtF;
368300130227         endif;
368400130227
368500130227         exsr  sr_ExecCmd;
368600130227
368700130227         //if  Qusei <> *blank;
368800130227         //  ...;
368900130227         //endif;
369000130227
369100130227         // -?Apertura file di stampa?
369200130227         open  PrtEmail;
369300130227
369400130227       ENDSR;
369500130227
369600130227       //--------------------------------------------------------------
369700130227       //?Chiusura file di stampa e cancellazione override.            ?
369800130227       //--------------------------------------------------------------
369900130227       BEGSR  sr_DeleteOvr;
370000130227
370100130227         // -?Chiusura file di stampa?
370200130227         if  %open(PrtEMAIL);
370300130227           close  PrtEMAIL;
370400130227         endif;
370500130227
370600130227         // -?Cancellazione override al file di stampa?
370700130227         Qcmd = c_CmdDltOvr;
370800130227         exsr  sr_ExecCmd;
370900130227
371000130227       ENDSR;
371100130227
371200130227       //--------------------------------------------------------------
371300130227       //?Esecuzione del comando (già impostato).                      ?
371400130227       //--------------------------------------------------------------
371500130227       BEGSR  sr_ExecCmd;
371600130227
371700130227         clear Qcap0100;
371800130227         Qcabcsdh = *off;
371900130227         Qcapa    = *off;
372000130227         Qcacmdss = *off;
372100130227         Qcaerved = *allX'00';
372200130227
372300130227         clear Qusec;
372400130227         Qusbprv  = %size(Qusec);
372500130227
372600130227         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
372700130227                           %size(Qcap0100) : 'CPOP0100' : *omit :
372800130227                           0 : 0 : Qusec);
372900130227
373000130227         //if  Qusei <> *blank;
373100130227         //  ...;
373200130227         //endif;
373300130227
373400130227       ENDSR;
373500101221
373600080724      /end-free
373700030515      *
373800030515      *---------------------------------------------------------------*
373900050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
374000030515      *---------------------------------------------------------------*
374100030515     c     sr_DS_ARB     BEGSR
374200030515      *
374300030515     c                   eval      DB0cbo = ARBcbo
374400030515     c                   eval      DB0tsp = ARBtsp
374500030515     c                   eval      DB0mgs = ARBmgs
374600030515     c                   eval      DB0tc1 = ARBtc1
374700030515     c                   eval      DB0tc2 = ARBtc2
374800030515     c                   eval      DB0fns = ARBfns
374900030515     c                   eval      DB0rmn = ARBrmn
375000030515     c                   eval      DB0rma = ARBrma
375100030515     c                   eval      DB0ias = ARBias
375200030515     c                   eval      DB0vas = ARBvas
375300030515     c                   eval      DB0vmd = ARBvmd
375400030515     c                   eval      DB0vad = ARBvad
375500030515     c                   eval      DB0qft = ARBqft
375600030515     c                   eval      DB0lna = ARBlna
375700030515     c                   eval      DB0znc = ARBznc
375800030515     c                   eval      DB0rsm = ARBrsm
375900030515     c                   eval      DB0inm = ARBinm
376000030515     c                   eval      DB0cam = ARBcam
376100030515     c                   eval      DB0lom = ARBlom
376200030515     c                   eval      DB0prm = ARBprm
376300030515     c                   eval      DB0nzm = ARBnzm
376400030515     c                   eval      DB0fap = ARBfap
376500030515     c                   eval      DB0rmo = ARBrmo
376600030515     c                   eval      DB0rsd = ARBrsd
376700030515     c                   eval      DB0ind = ARBind
376800030515     c                   eval      DB0cad = ARBcad
376900030515     c                   eval      DB0lod = ARBlod
377000030515     c                   eval      DB0prd = ARBprd
377100030515     c                   eval      DB0nzd = ARBnzd
377200030515     c                   eval      DB0fin = ARBfin
377300030515     c                   eval      DB0gc1 = ARBgc1
377400030515     c                   eval      DB0gc2 = ARBgc2
377500030515     c                   eval      DB0nas = ARBnas
377600030515     c                   eval      DB0ncl = ARBncl
377700030515     c                   eval      DB0pkf = ARBpkf
377800030515     c                   eval      DB0fvf = ARBfvf
377900030515     c                   eval      DB0vlf = ARBvlf
378000030515     c                   eval      DB0fls = ARBfls
378100030515     c                   eval      DB0ncd = ARBncd
378200030515     c                   eval      DB0nca = ARBnca
378300030515     c                   eval      DB0xco = ARBxco
378400030515     c                   eval      DB0ffd = ARBffd
378500030515     c                   eval      DB0dcr = ARBdcr
378600030515     c                   eval      DB0hcr = ARBhcr
378700030515     c                   eval      DB0tcr = ARBtcr
378800030515     c                   eval      DB0fbr = ARBfbr
378900030515     c                   eval      DB0gga = ARBgga
379000030515     c                   eval      DB0gma = ARBgma
379100030515     c                   eval      DB0gva = ARBgva
379200030515     c                   move      ' '           DB0gva
379300030515     c                   clear                   DB0sop
379400030515     c                   eval      DB0ctr = ARBctr
379500030515     c                   clear                   DB0drt
379600030515     c                   clear                   DB0fpp
379700030515     c                   clear                   DB0nrt
379800030515     c                   clear                   DB0pdr
379900030515      *
380000030515      * Allega documento di trasporto
380100060524      * & Codice cliente da Assegnato
380200130924     c***?(già fatto:)?  clear                   dsBL4A
380300031029     c                   clear                   DB0bam
380400061107     c                   exsr      RepAR4_A
380500060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
380600031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
380700030515     c                   movel     'S'           DB0bam
380800060505     c                   endif
380900030515      *
381000030515      * Destinatario: vedo se esiste 2ª ragione sociale
381100061107     c                   clear                   DB0rd2
381200061107     c                   exsr      RepAR4_D
381300030515      *
381400030515      * Note
381500061107     c                   clear                   DB0nt1
381600061107     c                   clear                   DB0nt2
381700061107     c                   exsr      RepAR4_89
381800030515      *
381900030515     c                   ENDSR
382000030515      *
382100030515      *---------------------------------------------------------------*
382200050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
382300050217      *?   "generiche" (non da "1A")                                 ?*
382400030515      *---------------------------------------------------------------*
382500030515     c     sr_DS_TAB     BEGSR
382600030515      *
382700030515      * Carico le tabelle "generiche" (3A, TB, 5E)
382800030515     c                   exsr      CarTAB
382900030515      *
383000030515      * Codici bolla
383100030515     c                   eval      DB0tb1 = §3Atb1
383200030515     c                   eval      DB0tb2 = §3Atb2
383300030515     c                   eval      DB0dcb = §3Ades
383400030515      * Tipo servizio
383500030515if  1c                   if        DB0dts = *blanks
383600030515     c                   eval      DB0dts = §5Edes
383700030515     c                   eval      DB0fst = §5Efst
383800030515e   1c                   endif
383900030515      * Consegne particolari
384000030515if  1c                   if            DB0tc1 <> *blanks
384100030515     c                             and DB0dc1 =  *blanks
384200030515     c                   movel(p)  DB0tc1        TBLkey
384300030515     c                   exsr      Tab_1P
384400030515     c                   eval      DB0dc1 = §1Pdes
384500030515e   1c                   endif
384600030515if  1c                   if            DB0tc2 <> *blanks
384700030515     c                             and DB0dc2 =  *blanks
384800030515     c                   movel(p)  DB0tc2        TBLkey
384900030515     c                   exsr      Tab_1P
385000030515     c                   eval      DB0dc2 = §1Pdes
385100030515e   1c                   endif
385200030515      * Esenzione IVA
385300030515if  1c                   if            DB0cei <> *blanks
385400030515     c                             and DB0dce =  *blanks
385500030515     c                   eval      DB0dce =  §EIdes
385600030515e   1c                   endif
385700030515      *
385800030515     c                   ENDSR
385900060524      *
386000060524      *---------------------------------------------------------------*
386100060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
386200061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
386300060524      *---------------------------------------------------------------*
386400060524     c     RepAR4_A      BEGSR
386500060524      *
386600130924     c***?(già fatto:)?  clear                   dsBL4A
386700061107      *
386800060524     c                   eval      Wtrc = 'A'
386900060524     c     KLDVtrc       chain     FIAR4000
387000060524      *
387100060524if  1c                   if        %found(FIAR401L)
387200060524     c                   movel     AR4Not        dsBL4A
387300060524e   1c                   endif
387400060524      *
387500060524     c                   ENDSR
387600061107      *
387700061107      *---------------------------------------------------------------*
387800061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
387900061107      *?(2ª ragione sociale del destinatario)                        ?*
388000061107      *---------------------------------------------------------------*
388100061107     c     RepAR4_D      BEGSR
388200061107      *
388300130924     c***?NON esiste:?   clear                   dsBL4D
388400130924     c***?(già fatto:)?  clear                   DB0rd2
388500061107      *
388600061107     c                   eval      Wtrc = 'D'
388700061107     c     KLDVtrc       chain     FIAR4000
388800061107      *
388900061107if  1c                   if        %found(FIAR401L)
389000061107     c                   movel     AR4not        DB0rd2
389100061107e   1c                   endif
389200061107      *
389300061107     c                   ENDSR
389400061107      *
389500060626      *---------------------------------------------------------------*
389600060626      *?Reperimento dati da FIAR400F x trk "I"                       ?*
389700061107      *?(dati DPD)                                                   ?*
389800060626      *---------------------------------------------------------------*
389900061030     c     RepAR4_I      BEGSR
390000061030      *
390100130924     c***?(già fatto:)?  clear                   dsBL4I
390200061107      *
390300061030     c                   eval      Wtrc = 'I'
390400061030     c     KLDVtrc       chain     FIAR4000
390500061030      *
390600061030if  1c                   if        %found(FIAR401L)
390700061030     c                   movel     AR4not        dsBL4I
390800060626e   1c                   endif
390900061030      *
391000061030     c                   ENDSR
391100071008      *
391200071015      *---------------------------------------------------------------*
391300071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
391400110204      *?(dati aggiuntivi per O.R.M. abbinato alla spedizione)        ?*
391500071015      *---------------------------------------------------------------*
391600071015     c     RepAR4_M      BEGSR
391700071015      *
391800130924     c***?(già fatto:)?  clear                   dsBL4M
391900071015      *
392000071015     c                   eval      Wtrc = 'M'
392100071015     c     KLDVtrc       chain     FIAR4000
392200071015      *
392300071015if  1c                   if        %found(FIAR401L)
392400071015     c                   movel     AR4Not        dsBL4M
392500071015e   1c                   endif
392600071015      *
392700071015     c                   ENDSR
392800121018      *
392900121018      *---------------------------------------------------------------*
393000121018      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
393100121018      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
393200121018      *---------------------------------------------------------------*
393300121018     c     RepAR4_Q      BEGSR
393400121018      *
393500130924     c***?(già fatto:)?  clear                   dTA4Q
393600130924     c***?(già fatto:)?  clear                   DB0cpi
393700121018      *
393800121018     c                   eval      Wtrc = 'Q'
393900121018     c     KLDVtrc       chain     FIAR4000
394000121018      *
394100121018if  1c                   if        %found(FIAR401L)
394200121018     c                   movel     AR4not        dTA4Q
394300121018e   1c                   endif
394400121018      *
394500121018     c                   ENDSR
394600080523      /free
394700080523
394800080523       //--------------------------------------------------------------
394900080523       //?Reperimento dati da FIAR400F x trk "1"                       ?
395000080523       //?(nominativo del destinatario)                                ?
395100080523       //--------------------------------------------------------------
395200080523       BEGSR  RepAR4_1;
395300080523
395400101222         //clear  dsBL41;        *** NON esiste ***?
395500080523
395600080523         Wtrc = '1';
395700080523         chain  KLDVtrc  FIAR4000;
395800080523
395900080523         if  NOT  %found(FIAR401L);
396000080523           clear  AR4not;
396100080523         endif;
396200080523
396300150430         if  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
396400081104           Pfedkcn = AR4not;
396500081104         else;
396600081104           Pkcn    = AR4not;
396700081104         endif;
396800080523
396900080523       ENDSR;
397000080523
397100080523      /end-free
397200061107      *
397300061107      *---------------------------------------------------------------*
397400061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
397500061107      *?(note)                                                       ?*
397600061107      *---------------------------------------------------------------*
397700061107     c     RepAR4_89     BEGSR
397800061107      *
397900130924     c***?NON esiste:?   clear                   dsBL48
398000130924     c***?NON usata:?    clear                   dsBL49
398100130924     c***?(già fatto:)?  clear                   DB0nt1
398200130924     c***?(già fatto:)?  clear                   DB0nt2
398300061107      *
398400061107     c                   eval      Wtrc = '8'
398500061107     c     KLDVtrc       chain     FIAR4000
398600061107if  1c                   if        %found(FIAR401L)
398700061107     c                   movel     AR4not        DB0nt1
398800061107e   1c                   endif
398900061107      *
399000061107     c                   eval      Wtrc = '9'
399100061107     c     KLDVtrc       chain     FIAR4000
399200061107if  1c                   if        %found(FIAR401L)
399300061107     c                   movel     AR4not        DB0nt2
399400061107e   1c                   endif
399500061107      *
399600061107     c                   ENDSR
399700030515      *
399800030515      *---------------------------------------------------------------*
399900050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
400000030515      *---------------------------------------------------------------*
400100030515     c     CarTAB        BEGSR
400200030515      *
400300030515      * Tab. "3A" - Codici Bolla
400400030515      * (NO se rimesso mittente)
400500030515     c                   clear                   ds3A
400600030515if  1c                   if        DB0ris <> 'M'
400700030515     c                   movel     '3A'          TBLcod
400800030515     c                   movel(p)  DB0cbo        TBLkey
400900030515     c     K03TAB        chain     TABEL
401000030515if  2c                   if        %found(TABEL00F)
401100030515     c                   movel     TBLuni        ds3A
401200030515e   2c                   endif
401300030515e   1c                   endif
401400070515     c                   eval      $Mamma =  (§3Abcm = 'S')
401500030515      *
401600030515      * Tab. "TB" - Tipi Bolla
401700030515      * (NO se rimesso mittente)
401800030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
401900030515      * se no non stampo i dati fattura
402000030515     c                   clear                   dsTB
402100030515if  1c                   if        DB0ris <> 'M'
402200030515     c                   movel     'TB'          TBLcod
402300121029if  2c                   if        §3Atb2 <> *blanks
402400030515     c                   movel(p)  §3Atb2        TBLkey
402500030515x   2c                   else
402600030515     c                   movel(p)  §3Atb1        TBLkey
402700030515e   2c                   endif
402800030515     c     K03TAB        chain     TABEL
402900030515if  2c                   if        %found(TABEL00F)
403000030515     c                   movel     TBLuni        dsTB
403100030515e   2c                   endif
403200030515e   1c                   endif
403300030515      *
403400030515      * Tab. "5E" - Tipi Servizio
403500030515     c                   clear                   ds5E
403600090514if  1c                   if        DB0tsp <> *blanks
403700030515     c                   movel     '5E'          TBLcod
403800030515     c                   movel(p)  DB0tsp        TBLkey
403900030515     c     K03TAB        chain     TABEL
404000030515if  2c                   if        %found(TABEL00F)
404100030515     c                   movel     TBLuni        ds5E
404200030515e   2c                   endif
404300030515e   1c                   endif
404400030515      *
404500030911      * Tab. "EI" - Esenzioni I.V.A.
404600030515     c                   clear                   dsEI
404700030911      * Tab. "1A" - Tipi incasso contrassegno
404800030911     c                   clear                   ds1A
404900030911      * Tab. "1P" - Consegne particolari
405000030911     c                   clear                   ds1P
405100030515      *
405200030515     c                   ENDSR
405300030911      *
405400030911      *---------------------------------------------------------------*
405500050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
405600030911      *---------------------------------------------------------------*
405700030911     c     Tab_EI        BEGSR
405800030911      *
405900030911     c                   clear                   dsEI
406000030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
406100030911     c                   movel     'EI'          TBLcod
406200030911     c                   movel(p)  DB0cei        TBLkey
406300030911     c     K03TAB        chain     TABEL
406400030911if  2c                   if        %found(TABEL00F)
406500030911     c                   movel     TBLuni        dsEI
406600030911e   2c                   endif
406700030911e   1c                   endif
406800030911     c                   eval      DB0dce =  §EIdes
406900030911      *
407000030911     c                   ENDSR
407100030515      *
407200030515      *---------------------------------------------------------------*
407300050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
407400030515      *---------------------------------------------------------------*
407500030515     c     Tab_1A        BEGSR
407600030515      *
407700030515     c                   clear                   ds1A
407800030515if  1c                   if        DB0cas <> *zeros
407900030515     c                   movel     '1A'          TBLcod
408000030515     c                   movel(p)  DB0tic        TBLkey
408100030515     c     K03TAB        chain     TABEL
408200030515if  2c                   if        %found(TABEL00F)
408300030515     c                   movel     TBLuni        ds1A
408400030515e   2c                   endif
408500030515e   1c                   endif
408600030515      *
408700030515      * Imposto i dati nella DS
408800030515     c                   eval      DB0dti = §1Ades
408900030515     c                   eval      DB0ucm = §1Aucm
409000030515     c                   eval      DB0ste = §1Aste
409100030515      *
409200030515     c                   ENDSR
409300030515      *
409400030515      *---------------------------------------------------------------*
409500050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
409600030515      *---------------------------------------------------------------*
409700030515     c     Tab_1P        BEGSR
409800030515      *
409900030515     c                   clear                   ds1P
410000030515     c                   movel     '1P'          TBLcod
410100130924     c***?già impostato:?movel(p)  DB0tc1/2      TBLkey
410200030515     c     K03TAB        chain     TABEL
410300030515if  1c                   if        %found(TABEL00F)
410400030515     c                   movel     TBLuni        ds1P
410500030515e   1c                   endif
410600030515      *
410700030515     c                   ENDSR
410800030515      *
410900030515      *---------------------------------------------------------------*
411000050217      *?Calcolo importi LDV                                          ?*
411100030515      *---------------------------------------------------------------*
411200061031     c     sr_DS_IMPO    BEGSR
411300030515      *
411400030515      * Vedo se esiste tassazione
411500030515      *  solo se tipo bolla contabilizzabile
411600061030if  1c                   IF        §TBfcb =  '1'
411700101213      *
411800061030     c                   eval      Wtrc   =  '1'
411900040607     c     KLDVtrc       chain     FIAR6000
412000101213      *
412100030515if  2c                   if        %found(FIAR601L)
412200030515     c                   movel     AR6div        DB0div
412300030515     c                   exsr      CtrDIV
412400030515      *
412500030515      * NON c'è 2ª bolla
412600101213if  3c                   if        DB0tb2 = *blanks
412700030515      *  Estremi fattura
412800030515     c                   z-add     AR6nft        DB0nft
412900030515     c                   z-add     AR6dft        DB0dft
413000130205     c                   z-add     AR6fiv        wB0fiv
413100101213      *  Se è assegnato => stampo tutta la tassaz.
413200101213if  4c                   if        %subst(DB0tb1:1:1) = 'A'
413300061030     c                   eval      Wtrc   =  '1'
413400030515     c                   exsr      RepTASSAZ
413500030515e   4c                   endif
413600030515e   3c                   endif
413700030515e   2c                   endif
413800030515      *
413900061030      * C'è 2ª bolla
414000101213if  2c                   IF        DB0tb2 <> *blanks
414100030515      *
414200061030     c                   eval      Wtrc   =  '2'
414300040607     c     KLDVtrc       chain     FIAR6000
414400030515      *
414500061030if  3c                   If        %found(FIAR601L)
414600030515     c                   movel     AR6div        DB0div
414700030515     c                   exsr      CtrDIV
414800030515     c                   movel     AR6cei        DB0cei
414900030515      *   Estremi fattura
415000030515     c                   z-add     AR6nft        DB0nft
415100030515     c                   z-add     AR6dft        DB0dft
415200130205     c                   z-add     AR6fiv        wB0fiv
415300030515if  4c                   if        AR6ksc <> *zeros
415400030515     c                   eval      DB0ccd =  AR6ksc
415500030515e   4c                   endif
415600061030     c                   eval      Wtrc   =  '2'
415700030515     c                   exsr      RepTASSAZ
415800061030e   3c                   EndIf
415900030515      *
416000030515e   2c                   ENDIF
416100030515      *
416200030515e   1c                   ENDIF
416300030515      *
416400030515      * Vedo se ha contrassegno
416500060505     c     KLDV          chain     FIAR9000
416600060505if  1c                   IF        %found(FIAR901L)
416700061030     c                   eval      DB0cas =  AR9cas
416800061030     c                   eval      DB0vca =  AR9vca
416900061030     c                   eval      DB0gca =  AR9gca
417000061030     c                   eval      DB0tic =  AR9tic
417100030515x   1c                   ELSE
417200030515     c                   clear                   DB0cas
417300030515     c                   clear                   DB0vca
417400030515     c                   clear                   DB0gca
417500030515     c                   clear                   DB0tic
417600030515     c                   clear                   DB0dti
417700030515     c                   clear                   DB0ucm
417800030515     c                   clear                   DB0ste
417900030515e   1c                   ENDIF
418000121018      *
418100131015      * Se P.A. con fattura immediata riporterò il Codice Fiscale
418200121018      *   (NON se destinatario estero)
418300121026if  1c                   if           ( %subst(DB0tb1:1:1) = 'A'
418400121018     c                             and (DB0nft  <> *zeros
418500121018     c                              or  DB0ift  <> *zeros)
418600121018     c                             and  DB0nzd  =  *blanks )
418700121018     c                   exsr      RepAR4_Q
418800121018     c                   movel     §TA4Qcfd      DB0cpi
418900121018e   1c                   endif
419000121018     c                   eval      wB0cfi = DB0cpi
419100061030      *
419200030515      * Vedo se si tratta di bolla figlia:
419300040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
419400070515if  1c                   if        $Mamma =  *on
419500030911     c                   exsr      BollaFiglia
419600030515e   1c                   endif
419700030515      *
419800030515     c                   ENDSR
419900030515      *
420000030515      *---------------------------------------------------------------*
420100050217      *?Decodifica LNA                                               ?*
420200030515      *---------------------------------------------------------------*
420300131203     c     sr_DecLNA     BEGSR
420400030515      *
420500030515if  1c                   if            DB0dla =  *blanks
420600030515     c                             and DB0ris <> 'M'
420700030515      *
420800040311     c                   eval      SAVlna = DB0lna
420900070312     c                   clear                   OG146arr
421000061016     c                   clear                   OG148arr
421100070312     c                   clear                   OG150arr
421200030515     c     DB0lna        chain     AZORG
421300030515      *
421400030515if  2c                   if        %found(AZORG01L)
421500030515     c                   eval      DB0fag = ORGfag
421600030515     c                   eval      DB0dla = ORGdes
421700030515     c                   eval      DB0inl = ORGind
421800030515     c                   eval      DB0cal = ORGcpf
421900030515     c                   eval      DB0lol = ORGloc
422000030515     c                   eval      DB0prl = ORGpro
422100030515     c                   eval      DB0tel = ORGtel
422200030515     c                   eval      DB0fax = ORGfax
422300070312     c                   movel     ORGde6        OG146arr
422400061016     c                   movel     ORGde8        OG148arr
422500070312     c                   movel     ORGdf0        OG150arr
422600030515e   2c                   endif
422700030515      *
422800030515e   1c                   endif
422900030515      *
423000030515     c                   ENDSR
423100030515      *
423200030515      *---------------------------------------------------------------*
423300050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
423400030515      *---------------------------------------------------------------*
423500030515     c     RepTASSAZ     BEGSR
423600030515      *
423700030515     c                   eval      DB0imv = AR6imv
423800030515     c                   eval      DB0cei = AR6cei
423900030515     c                   eval      DB0ift = AR6ift
424000101213      *
424100030911      * Esenzione IVA
424200030911if  1c                   if            DB0cei <> *blanks
424300030911     c                             and DB0dce =  *blanks
424400030911     c                   exsr      Tab_EI
424500030911e   1c                   endif
424600101213      *
424700101213      * Carico tabella VARIABILI ARRIVI
424800101213     c                   clear                   ds$2
424900101213     c                   eval      TBLcod = '$2'
425000101213     c                   eval      TBLkey = '1'
425100101213     c     K03TAB        chain     TABEL
425200101213if  1c                   if        %found(TABEL00F)
425300101213     c                   movel     TBLuni        ds$2
425400101213e   1c                   endif
425500030515      *
425600030515      * Controllo se ci sono varie esenti
425700030515      *
425800030515      * - varia 1
425900030515     c                   eval      Wsvx = AR6sv1
426000030515     c                   eval      Wvax = AR6va1
426100030515     c                   exsr      RepTASSAZ_b
426200030515      * - varia 2
426300030515     c                   eval      Wsvx = AR6sv2
426400030515     c                   eval      Wvax = AR6va2
426500030515     c                   exsr      RepTASSAZ_b
426600030515      * - varia 3
426700030515     c                   eval      Wsvx = AR6sv3
426800030515     c                   eval      Wvax = AR6va3
426900030515     c                   exsr      RepTASSAZ_b
427000030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
427100040607     c     KLDVtrc       setll     FIAR7000
427200040607     c     KLDVtrc       reade     FIAR7000
427300030515      *
427400030515do  2c                   Dow       Not %eof(FIAR701L)
427500030515      *
427600030515     c                   eval      Wsvx = AR7svn
427700030515     c                   eval      Wvax = AR7van
427800030515     c                   exsr      RepTASSAZ_b
427900030515      *
428000040607     c     KLDVtrc       reade     FIAR7000
428100030515e   2c                   Enddo
428200030515      *
428300030515     c                   ENDSR
428400030515      *
428500030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
428600050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
428700050217      *?- 2ª parte -                                                 ?*
428800030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
428900030515     c     RepTASSAZ_b   BEGSR
429000030515      *
429100030515sel 1c                   Select
429200030515      *
429300030515w   1c                   When      Wsvx       = *blanks
429400030515      *
429500030515w   1c                   When      Wsvx       = §$2iva
429600030515     c                   eval      DB0iva     = Wvax
429700030515      *
429800030515w   1c                   When      Wsvx       = §$2bol
429900030515     c                   add       Wvax          DB0ese
430000110117     c*//                z-add     Wvax          wBolloVirt
430100110117     c*//                eval      $BolloVirt = *on
430200030515      *
430300030515x   1c                   Other
430400101213      * Reperimento varie esenti IVA
430500101213     c                   movel     'CC'          TBLcod
430600101213     c                   movel(p)  Wsvx          TBLkey
430700101213     c     K03TAB        chain     TABEL
430800101213if  1c                   IF             %found(TABEL00F)
430900101213     c                             and  TBLflg = *blanks
431000101213     c                   movel     TBLuni        dsCC
431100101213if  2c                   if        §CCcei <> *blanks
431200101213     c                   add       Wvax          DB0ese
431300101213     c                   sub       Wvax          DB0imv
431400101213e   2c                   endif
431500101213e   1c                   ENDIF
431600030515      *
431700101213e   1c                   Endsl
431800030515      *
431900030515     c                   ENDSR
432000030515      *
432100030515      *---------------------------------------------------------------*
432200050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
432300030515      *---------------------------------------------------------------*
432400030515     c     TABxPAR       BEGSR
432500030515      *
432600100517     c                   movel(p)  WCdPar        w053a
432700030515     c                   movel     '7V'          TBLcod
432800030515     c                   movel(p)  KeyPAR        TBLkey
432900030515     c     K03TAB        chain     TABEL
433000030515      *
433100030515if  1c                   if        %found(TABEL00F)
433200030515     c                             and TBLflg = *blanks
433300100517     c                   eval      w053a = WCdPar + '-'
433400121108     c                                   + %trim(%subst(TBLuni:1:49))
433500030515x   1c                   else
433600030515     c                   clear                   TBLuni
433700030515e   1c                   endif
433800030515      *
433900030515     c                   ENDSR
434000030515      *
434100030515      *---------------------------------------------------------------*
434200050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
434300030515      *---------------------------------------------------------------*
434400030515     c     CtrCAs        BEGSR
434500030515      *
434600030515      * E' da convertire
434700030515if  1c                   if        DB0ucm = 'S'
434800030515     c                   eval      $CAeur = *on
434900030515e   1c                   endif
435000030515      *
435100030515      * Se in EURO è da convertire e da stampare in EURO
435200030515if  1c                   if        DB0vca = §GEDcn
435300030515     c                   eval      $CAeur = *on
435400030515     c                   endif
435500030515      *
435600030515      * Devo convertire il c/a da valuta a moneta di conto
435700030624if  1c                   if        DB0vca <> §GEDcn
435800030515     c                   clear                   YEURCODS
435900030515     c                   movel     DB0vca        YECdvi
436000030515     c                   z-add     DB0cas        YECimi
436100030515     c                   movel     §GEDcn        YECdvo
436200030515     c                   call      'YEURCO'
436300030515     c                   parm                    YEURCODS
436400030515if  2c                   if        YECesi = *blanks
436500030515     c                   z-add     YECimo        Wcas
436600030515e   2c                   endif
436700030515x   1c                   else
436800030515     c                   z-add     DB0cas        Wcas
436900030515e   1c                   endif
437000030515      *
437100030515     c                   ENDSR
437200030515      *
437300030515      *---------------------------------------------------------------*
437400050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
437500030515      *---------------------------------------------------------------*
437600030515     c     CtrDIV        BEGSR
437700030515      *
437800030515     c                   clear                   dsCV
437900030515      *
438000030515     c                   movel     'CV'          TBLcod
438100030515     c                   movel(p)  DB0div        TBLkey
438200030515     c     K03TAB        chain     TABEL
438300030515     c                   if        %found(TABEL00F)
438400030515     c                   movel     TBLuni        dsCV
438500030515     c                   movel     §CVfdc        DB0fdc
438600030515     c                   endif
438700030515      *
438800030515     c                   ENDSR
438900030515      *
439000030515      *---------------------------------------------------------------*
439100050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
439200030515      *---------------------------------------------------------------*
439300030911     c     BollaFiglia   BEGSR
439400030515      *
439500090619     c                   if        not %open(FNLBL01L)
439600101213     c                   open(e)   FNLBL01L
439700101213      /free
439800101213           if  %error;
439900101213             exsr  sr_ErrOpenFile;
440000101213           endif;
440100101213      /end-free
440200090619     c                   endif
440300090619      *
440400040607     c     KLDV          chain     FNLBL000
440500030515if  1c                   if        %found(FNLBL01L)
440600030515      *
440700030515     c     KLBL          chain     FNARB000
440800030515if  2c                   if        %found(FNARB01L)
440900030515      *
441000031015     c                   eval      wB0nrs = ARBnrs
441100030515     c                   eval      DB0fns = ARBfns
441200030515     c                   eval      DB0fls = ARBfls
441300030515     c                   eval      DB0ncd = ARBncd
441400030515     c                   eval      DB0nca = ARBnca
441500050114     c**!!!              eval      DB0xco = ARBxco
441600030515     c                   eval      DB0znc = ARBznc
441700030515      * Aggancio AR9
441800060505     c     KLBL          chain     FIAR9000
441900060505if  3c                   IF        %found(FIAR901L)
442000030515     c                   eval      DB0cas = AR9cas
442100030515     c                   eval      DB0vca = AR9vca
442200030515     c                   eval      DB0gca = AR9gca
442300030515     c                   eval      DB0tic = AR9tic
442400030515x   3c                   ELSE
442500030515     c                   clear                   DB0cas
442600030515     c                   clear                   DB0vca
442700030515     c                   clear                   DB0gca
442800030515     c                   clear                   DB0tic
442900030515     c                   clear                   DB0dti
443000030515     c                   clear                   DB0ucm
443100030515     c                   clear                   DB0ste
443200030515e   3c                   ENDIF
443300030515      *
443400030515e   2c                   endif
443500030515      *
443600030515e   1c                   endif
443700030515      *
443800030515     c                   ENDSR
443900030903      *
444000030515      *---------------------------------------------------------------*
444100050217      *?DEFINIZIONE KEY-LIST                                         ?*
444200030515      *---------------------------------------------------------------*
444300030515     c     DefKLIST      BEGSR
444400030515      *
444500090619      * - FNARB01L / FNART01L /
444600030515      *   FNLBL01L /
444700060505      *   FIAR501L/31C / FIAR901L
444800040607     c     KLDV          klist
444900030515     c                   kfld                    DB0aas
445000030515     c                   kfld                    DB0lnp
445100030515     c                   kfld                    DB0nrs
445200030515     c                   kfld                    DB0nsp
445300060505      * - FIAR401L / FIAR601L / FIAR701L
445400060505     c     KLDVtrc       klist
445500060505     c                   kfld                    DB0aas
445600060505     c                   kfld                    DB0lnp
445700060505     c                   kfld                    DB0nrs
445800060505     c                   kfld                    DB0nsp
445900060505     c                   kfld                    Wtrc
446000040607      * - FIAR501L/31C
446100030717     c     K05AR501      klist
446200030717     c                   kfld                    DB0aas
446300030717     c                   kfld                    DB0lnp
446400030717     c                   kfld                    DB0nrs
446500030717     c                   kfld                    DB0nsp
446600030717     c                   kfld                    AR5trd
446700090619      * - FNARB01L / FIAR901L di bolla MAMMA
446800070514     c     KLBL          klist
446900070514     c                   kfld                    LBLaap
447000070514     c                   kfld                    LBLlpp
447100070514     c                   kfld                    LBLnrp
447200070514     c                   kfld                    LBLnsp
447300081030      * - FNART01L di bolla MAMMA
447400081030     c     KLBLnsc       Klist
447500081030     c                   kfld                    LBLaap
447600081030     c                   kfld                    LBLlpp
447700081030     c                   kfld                    LBLnrp
447800081030     c                   kfld                    LBLnsp
447900081030     c                   Kfld                    Wnsc
448000081030     c     K05ARS01A     klist
448100081030     c                   kfld                    ARTfls
448200081030     c                   kfld                    ARTlna
448300081030     c                   kfld                    ARTnrs
448400081030     c                   kfld                    ARTnsc
448500081030     c                   kfld                    ARStrc
448600081030      * - FNART01L
448700081030     c     KLDVnsc       Klist
448800081030     c                   Kfld                    DB0aas
448900081030     c                   Kfld                    DB0lnp
449000081030     c                   Kfld                    DB0nrs
449100081030     c                   Kfld                    DB0nsp
449200081030     c                   Kfld                    Wnsc
449300100517      * - FNORM01L
449400100517     c     K04ORM01      klist
449500100517     c                   kfld                    ORMpoe
449600100517     c                   kfld                    ORMnsr
449700100517     c                   kfld                    ORMnor
449800100517     c                   kfld                    ORMnrv
449900030515      * - TABEL00F
450000030515     c     K03TAB        klist
450100040223     c                   kfld                    TBLkut
450200030515     c                   kfld                    TBLcod
450300030515     c                   kfld                    TBLkey
450400030515      *
450500030515     c                   ENDSR
450600080430
450700080430       //--------------------------------------------------------------
450800140303       //?Operazioni finali                                            ?
450900080430       //--------------------------------------------------------------
451000080430     c     sr_End        BEGSR
451100080430      *
451200080430     c                   clear                   TIBS02ds
451300080430     c                   eval      T02tla = 'C'
451400080430     c                   call      'TIBS02R'
451500080430     c                   parm                    KPJBA
451600080430     c                   parm                    TIBS02ds
451700080430     c                   reset                   FNLV80ds
451800080430     c                   call      'FNLV80R'
451900080430     c                   parm                    KPJBA
452000080430     c                   parm                    FNLV80ds
452100080430      /free
452200101222         if  §VPOrcm <= wDate and
452300101222             DB0tsp   = 'E'   and
452400101222            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
452500101222                                   DB0lnp <> 006   and
452600101222                                   DB0lnp <> 063));
452700101222           reset  TNSD99ds;
452800101222           tnsd99r ( TNSD99ds );
452900110117         endif;
453000080724
453100101222         // -?Chiusura del work-file?- - - - - - - - - - - - - - - - -*
453200090210
453300110202         if  %open(FIILV01L);
453400110202           close  FIILV01L;
453500090210         endif;
453600080430
453700101222         // -?Restituzione parametri?- - - - - - - - - - - - - - - - -*
453800080508
453900110221         if wCod_Log  <> '01';
454000130206           RootFolder = §ILVimgSca;
454100110221         endif;
454200110221
454300080520         if Risultato <> 'OK';
454400080718
454500080731           clear FileName;
454600080731           clear TemplateName;
454700080508
454800130213           for wx = 1 to c_Row;
454900130206             %occur(LdV) = wx;
455000130206             LdvPrg      = wx;
455100130206           endfor;
455200080508
455300080508         endif;
455400080430
455500131203
455600080430           exec sql   set result sets
455700080430                      for return to client
455800081103                      array :LdV   for :w_row rows;
455900080430
456000080430           select;
456100080430             when sqlCode = 100;
456200080430               RtnStatus  = sqlCode;
456300130924               //RtnOpCode  = 'SQLCODE   ';
456400130924               //RtnEsito   = -1;
456500080430             when sqlCode < *zero;
456600080430               RtnStatus  = sqlCode;
456700130924               //RtnOpCode  = 'SQLCODE   ';
456800130924               //RtnEsito   = -1;
456900080430           endsl;
457000080430
457100080430
457200080430         return;
457300080430
457400080430      /end-free
457500080430     c                   ENDSR
457600080430
457700080430      *---------------------------------------------------------------*
457800130227
457900140303       //--------------------------------------------------------------
458000140303       //?Spool di stampa (per e-mail).                                ?
458100140303       //--------------------------------------------------------------
458200130227
458300130227     oPrtEMAIL  e            EmlTxt            2
458400130227     o                       SDSpgm
458500130227     o                                        +   1 '-'
458600130227     o                                        +   1 'SPEDIZIONE GIÀ EL-
458700130227     o                                              ABORATA 100 VOLTE'
458800130227     o          e            EmlTxt      0
458900130227     o                       SDSpgm
459000130227     o                                        +   1 '-'
459100130227     o                                        +   1 'SPEDIZIONE GIÀ EL-
459200130227     o                                              ABORATA 100 VOLTE'
459300130227     o          e            EmlTxt      2
459400130227     o                                              'Spedizione     '
459500130227     o                                        +   3 'Distinta  '
459600130227     o          e            EmlTxt      1  1
459700130227     o                                              '------------------
459800130227     o                                              --'
459900130227     o                                        +   3 '----------'
460000130227     o          e            EmlDet      1
460100130227     o                       DB0aas        3
460200130227     o                       DB0lnp        3  +   1
460300130227     o                       DB0nrs        3  +   1
460400130227     o                       DB0nsp        3  +   1
460500130227     o                       P_Fgs2        3  +   3
460600130227     o                       P_Ndc2        3  +   1
460700130227     o          e            EmlEnd      2
460800130227     o                                              '***  Fine Lista  ***'
460900130227     o          e            EmlEnd      0
461000130227     o                                              '***  Fine Lista  ***'
461100080724
461200140303       //--------------------------------------------------------------
461300140303       //?Spool per immagine della LdV.                                ?
461400140303       //--------------------------------------------------------------
461500080430     oQSYSPRT   e            PRTtxt            3
461600080430     o                                              '   '
461700080430     o                       c_Char           +   0
461800080430     o          e            PRTrow      1
461900080430     o                       LDV              +   0
462000110201     o          e            PRTend      3
462100110201     o                       LastRow          +   0
462200080430
462300080718       //--------------------------------------------------------------
462400140303       //?Procedure name: getCurrentSystemName.                        ?
462500140303       //?Purpose:        Reperisce l'attributo di rete current System Name?
462600140303       //?Returns:        Current System Name                          ?
462700080718       //--------------------------------------------------------------
462800080718     p getCurrentSystemName...
462900080718     p                 b
463000080718
463100080718     d getCurrentSystemName...
463200080718     d                 pi             8a
463300080718
463400080718      /free
463500080718
463600080718       clear qUSEC;
463700080718       qUSBPRV = %size(qUSEC);
463800080718
463900080718       RetrieveNetworkAttributes( QWCRNETA_dataReturned
464000080718                                : %size(qWCRNETA_dataReturned)
464100080718                                : 1
464200080718                                : 'SYSNAME   '
464300080718                                : qUSEC
464400080718                                );
464500080718
464600080718       if  qUSEI <> *blank   or
464700080718           qWCRNETA_dataReturned.qWCRDR.qWCNBRAR = *zero;
464800080718         dump(A);
464900080718         clear qWCRNETA_networkAttributeInformationTable;
465000080718       else;
465100080718         qWCRNETA_networkAttributeInformationTable =
465200080718                                       qWCRNETA_dataReturned.qWCARRAY;
465300080718       endif;
465400080718
465500080718       return  qWCRNETA_networkAttributeInformationTable.qWCDATA;
465600080718
465700080718      /end-free
465800080718
465900080718     p getCurrentSystemName...
466000080718     p                 e
466100080718
466200110202       //--------------------------------------------------------------
466300110202       //?Schiere a tempo di compilazione.                             ?
466400110202       //--------------------------------------------------------------
466500030515
466600030515** > $GgS/$Gio * --------------------------------------------------------------*
4667000306241LUNEDI
4668000306242MARTEDI
4669000410263MERCOLEDI
4670000306244GIOVEDI
4671000306245VENERDI
4672000305156SABATO
4673000305157DOMENICA
467400030515M          MATT.
467500030515P          POM.
467600110202** -?$CoL?(Codice LOG)?/?$DeL?(Decodifica codice LOG)?
46770011020200Spedizione in distinta chiusa - da elaborare
46780011020201Spedizione elaborata
46790011032808Sped. non consegnata in dist. - immagine a SCARTO
46800011032509LdV da scannerizzare - immagine a SCARTO
46810011020321Parametri di input errati - immagine NON creata
46820011020822Distinta non trovata/annullata - immagine a SCARTO
46830011020823Sped./Dist. non trovata in FIILV - immag. a SCARTO
46840011020325Sped. non trovata in FNARB - immagine NON creata
46850013031351BOLLA RECUPERO - Immagine già generata
46860013031352BOLLA RECUPERO - Spedizione non trovata in FNARB
