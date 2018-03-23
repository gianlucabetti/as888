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
009000170517     fTFNTC01L  if   e           k disk    usropn
009100110117
009200131017       // -?Dati per LdV partenza (solo per bolla mamma - cliente CLN)?
009300110117     fFNBLP01L  if   e           k disk    usropn
009400101213
009500101207       // -?O.R.M.?
009600100517     fFNORM01L  if   e           k disk    usropn
009700101213
009800110309       // -?File LOG spedizioni elaborate per immagine LdV?
009900110202     fFIILV01L  Uf A e           k disk    usropn
010000130227
010100130227       // -?Spool da inviare via e-mail:?
010200130227       //   ·?Corpo del messaggio di posta elettronica?
010300130227     fPrtEMAIL  o    f  132        printer usropn  oflind(*inOE)
010400101213
010500101207       // -?Printer File  (se richiesta bozza in stampa)?
010600090210     fQSYSPRT   o    f  132        printer usropn  oflind(*inOF)
010700030515
010800101213       //--------------------------------------------------------------
010900101213       //?Definizione costanti.                                        ?
011000101213       //--------------------------------------------------------------
011100030515
011200110117     d c_DateMax_A     c                   const('20391231')
011300170517       // -?Capoconto clienti?
011400170517     d c_Kci           c                   const(0151)
011500131008       // -?Costanti in output?
011600110117     d c_ScNS          c                   const('ELENCO SEGNACOLLI -
011700030515     d                                     NON SEQUENZIALI')
011800110117     d c_CoOr          c                   const('COLLI ORIGINALI     -
011900030717     d                                     N. ')
012000110117     d c_PMxA          c                   const('PROMEMORIA PER L''AU-
012100070315     d                                     TOTRASPORTATORE')
012200101213       // -?Codice cliente "FedEx"?
012300150430     d c_CliFedEx      c                   const(0501331)
012400150430     d c_CliFedEx2     c                   const(0506514)
012500101213       // -?Codice cliente "PraOil oleodotti italiani"?
012600150430     d c_CliPraOil     c                   const(0201515)
012700101213       // -?Codice cliente "TTL"?
012800091116     d c_CliTTL        c                   const(0504450)
012900101213       // -?Codice cliente "Lotterie Nazionali - Gratta & Vinci"?
013000090619     d c_CliCLN        c                   const(0632910)
013100140513     d c_CliLN_2       c                   const(0017093)
013200101213       // -?Codice cliente "S G Soluzioni Logistiche"?
013300090619     d c_CliSG         c                   const(0634923)
013400091117     d c_CliSG_2       c                   const(0635042)
013500110923
013600110923       // -?Consegna CON Riserva specifica?
013700110923     d c_RiservaSpecif...
013800110923     d                 c                   const('RS')
013900091117
014000131008       // -?Costante per evitare l'indicazione del contratto x "S.G."?
014100091117     d c_NoContr       c                   const('NO CONTRATTO')
014200101213
014300101213       // -?Costante per controllo "caratteri solo numerici"?
014400110126     d c_Digits        c                   const('0123456789')
014500101207       // -?Numero di righe da gestire per la LdV?
014600130213     d c_Row           c                   const(36)
014700081027     d c_Row_B         c                   const(49)
014800130213     d c_Row_B2        c                   const(51)                            c_Row_B + 2
014900081030     d c_Row_Fed       c                   const(10)
015000101207       // -?Sequenza numerica?
015100080430     d c_Char          c                   const('*...+....1....+....2+
015200080430     d                                            ....+....3....+....4+
015300080430     d                                            ....+....5....+....6+
015400080430     d                                            ....+....7....+....8+
015500080430     d                                            ....+....9....+...10+
015600080430     d                                            ....+...11....+...12')
015700130227
015800130227       // -?Dati di default?
015900130227     d c_Mittente      c                   const('ced')
016000130227     d c_at            c                   const('@brt.it')
016100130227     d c_Sede          c                   const('046')
016200130227
016300130227       // -?Comandi di override al PrtF "corpo e-mail"?
016400130227     d c_CmdOvrPrtF    c                   const('OVRPRTF +
016500130227     d                                           file(PRTEMAIL) +
016600130227     d                                           pagesize(66 132) +
016700130227     d                                           lpi(6) cpi(10) +
016800130227     d                                           ovrscope(*actgrpdfn) +
016900130227     d                                           ')
017000130227     d c_CmdDltOvr     c                   const('DLTOVR +
017100130227     d                                            file(PRTEMAIL) +
017200130227     d                                            lvl(*actgrpdfn)')
017300101213
017400101213       //--------------------------------------------------------------
017500101213       //?Definizione schiere.                                         ?
017600101213       //--------------------------------------------------------------
017700101213
017800101213       // -?Giorni della settimana e relativa decodifica?
017900070306     d $GgS            s              1    dim(9)   ctdata  perrcd(1)
018000070306     d $Gio            s             15    dim(9)   alt($GgS)
018100110202
018200110202       // -?Codice / Decodifica LOG x FIILV00F?
018300110202     d $CoL            s                   like(ILVcol)
018400130313     d                                     dim(10)  ctdata  perrcd(1)
018500110202     d $DeL            s                   like(ILVnot)
018600130313     d                                     dim(10)  alt($CoL)
018700101213
018800101207       // -?Dati da restituire?
018900130214     d LdV             ds           123    occurs(c_Row_B2) inz
019000080508     d   LdvPrg                       3s 0 inz
019100080430     d   LdvTxt                     120    inz
019200101213
019300101213       // -?Note e Particolarità?
019400070306     d $NP             s                   dim(6)   inz  like(Pnp1)
019500101213       // -?Consegne Precedenti?
019600070319     d $CP             s                   dim(20)  inz(*all'9')
019700070319     d                                                   like(Elem_CP)
019800070319     d $CPdcm          s                   dim(20)  inz  like(ARTdcm)
019900101213
020000101213       //--------------------------------------------------------------
020100101213       //?Definizione strutture dati.                                  ?
020200101213       //--------------------------------------------------------------
020300101213
020400080430     d KPJBA         e ds                  inz
020500101213
020600131008       // -?Dati LDV da restituire per l'immagine?
020700080506     d FNLSB5ds      e ds                  inz
020800080430
020900110202       // -?Records in output?
021000110202       // ·? 1ª riga?
021100081021     d ds_01         e ds                  inz  extname(FISP3001ds)
021200081021     d                                          prefix(P : 3)
021300110202       // ·? 2ª riga (vuota)?
021400110202       // ·? 3ª riga?
021500080430     d ds_03         e ds                  inz  extname(FISP3003ds)
021600080430     d                                          prefix(P : 3)
021700110202       // ·? 4ª riga?
021800080430     d ds_04         e ds                  inz  extname(FISP3004ds)
021900080430     d                                          prefix(P : 3)
022000110202       // ·? 5ª riga?
022100080430     d ds_05         e ds                  inz  extname(FISP3005ds)
022200080430     d                                          prefix(P : 3)
022300110202       // ·? 6ª riga (vuota)?
022400110202       // ·? 7ª riga?
022500080430     d ds_07         e ds                  inz  extname(FISP3007ds)
022600080430     d                                          prefix(P : 3)
022700110202       // ·? 8ª riga (vuota)?
022800110202       // ·? 9ª riga?
022900080430     d ds_09         e ds                  inz  extname(FISP3009ds)
023000080430     d                                          prefix(P : 3)
023100110202       // ·?10ª riga?
023200080430     d ds_10         e ds                  inz  extname(FISP3010ds)
023300080430     d                                          prefix(P : 3)
023400110202       // ·?11ª riga?
023500080430     d ds_11         e ds                  inz  extname(FISP3011ds)
023600080430     d                                          prefix(P : 3)
023700110202       // ·?12ª riga?
023800080430     d ds_12         e ds                  inz  extname(FISP3012ds)
023900080430     d                                          prefix(P : 3)
024000110202       // ·?13ª riga?
024100080430     d ds_13         e ds                  inz  extname(FISP3013ds)
024200080430     d                                          prefix(P : 3)
024300110202       // ·?14ª riga?
024400080430     d ds_14         e ds                  inz  extname(FISP3014ds)
024500080430     d                                          prefix(P : 3)
024600130205     d ds_140        e ds                  inz  extname(FISP30140s)
024700130205     d                                          prefix(P : 3)
024800110202       // ·?15ª riga?
024900080430     d ds_15         e ds                  inz  extname(FISP3015ds)
025000080430     d                                          prefix(P : 3)
025100110202       // ·?16ª riga?
025200080430     d ds_16         e ds                  inz  extname(FISP3016ds)
025300080430     d                                          prefix(P : 3)
025400110202       // ·?17ª riga?
025500080430     d ds_17         e ds                  inz  extname(FISP3017ds)
025600080430     d                                          prefix(P : 3)
025700110202       // ·?18ª riga?
025800080430     d ds_18         e ds                  inz  extname(FISP3018ds)
025900080430     d                                          prefix(P : 3)
026000130205     d ds_180        e ds                  inz  extname(FISP30180s)
026100130205     d                                          prefix(P : 3)
026200110202       // ·?19ª riga?
026300080430     d ds_19         e ds                  inz  extname(FISP3019ds)
026400080430     d                                          prefix(P : 3)
026500110202       // ·?20ª riga?
026600080430     d ds_20         e ds                  inz  extname(FISP3020ds)
026700080430     d                                          prefix(P : 3)
026800110202       // ·?21ª riga?
026900080430     d ds_21         e ds                  inz  extname(FISP3021ds)
027000080430     d                                          prefix(P : 3)
027100110202       // ·?22ª riga?
027200080430     d ds_22         e ds                  inz  extname(FISP3022ds)
027300080430     d                                          prefix(P : 3)
027400110202       // ·?23ª riga?
027500080430     d ds_23         e ds                  inz  extname(FISP3023ds)
027600080430     d                                          prefix(P : 3)
027700110202       // ·?24ª riga?
027800080430     d ds_24         e ds                  inz  extname(FISP3024ds)
027900080430     d                                          prefix(P : 3)
028000110202       // ·?25ª riga?
028100080430     d ds_25         e ds                  inz  extname(FISP3025ds)
028200080430     d                                          prefix(P : 3)
028300110202       // ·?26ª riga (vuota)?
028400110202       // ·?27ª riga?
028500080430     d ds_27         e ds                  inz  extname(FISP3027ds)
028600080430     d                                          prefix(P : 3)
028700110202       // ·?28ª riga?
028800080430     d ds_28         e ds                  inz  extname(FISP3028ds)
028900080430     d                                          prefix(P : 3)
029000110202       // ·?29ª riga?
029100081021     d ds_29         e ds                  inz  extname(FISP3029ds)
029200081021     d                                          prefix(P : 3)
029300110202       // ·?30ª riga?
029400081021     d ds_30         e ds                  inz  extname(FISP3030ds)
029500081021     d                                          prefix(P : 3)
029600110215       // ·?31ª riga?
029700110215     d ds_31         e ds                  inz  extname(FISP3031ds)
029800110215     d                                          prefix(P : 3)
029900110202       // ·?32ª riga?
030000081021     d ds_32         e ds                  inz  extname(FISP3032ds)
030100081021     d                                          prefix(P : 3)
030200110202       // ·?33ª riga?
030300081021     d ds_33         e ds                  inz  extname(FISP3033ds)
030400081021     d                                          prefix(P : 3)
030500110202       // ·?34ª riga?
030600081021     d ds_34         e ds                  inz  extname(FISP3034ds)
030700081021     d                                          prefix(P : 3)
030800110202       // ·?35ª riga (vuota)?
030900110202       // ·?36ª riga (vuota)?
031000110202       // ·?37ª riga e successive?
031100081023     d ds_37         e ds                  inz  extname(FISP3037ds)
031200081021     d                                          prefix(P : 3)
031300110202       // ·?Righe x FedEx?
031400081030     d ds_Fed        e ds                  inz  extname(FISP304Fds)
031500081030     d                                          prefix(P : 3)
031600130411
031700130411       // -?Flag operativi in TITAS30C?
031800130411     d dTASflo       e ds                  inz     qualified
031900120702
032000130214       // -?Dati aggiuntivi in FIILV00F?
032100120702     d dILVlibero    e ds                  inz     qualified
032200101220
032300101220       // -?Parametri per conversione importi in Euro?
032400061107     d YEURCOds      e ds                  inz
032500110202
032600101220       // -?Dati da AZORG00F?
032700070424     d OG143ccm      e ds                  inz     extname(OG143)
032800070424     d                                             prefix(§CM:3)
032900070424     d OG146arr      e ds                  inz     extname(OG146)
033000070424     d                                             prefix(A)
033100070424     d OG146cf£6     e ds                  inz     extname(OG146)
033200070424     d                                             prefix(£6:1)
033300070424     d OG148arr      e ds                  inz     extname(OG148)
033400070424     d                                             prefix(A)
033500070424     d OG148cf£6     e ds                  inz     extname(OG148)
033600070424     d                                             prefix(£6:1)
033700070424     d OG150arr      e ds                  inz     extname(OG150)
033800070424     d                                             prefix(A)
033900110202
034000101220       // -?Dati da FIAR400F?
034100061108     d dsBL4A        e ds                  inz
034200061108     d dsBL4I        e ds                  inz
034300071015     d dsBL4M        e ds                  inz
034400121018     d dTA4Q         e ds                  inz
034500110228
034600110228       // -?Dati da FIDST00F?
034700110228     d dDSTflr       e ds                  inz
034800110202
034900101220       // -?Tabelle?
035000131206     d ds$2          e ds
035100061107     d ds1A          e ds                  inz
035200061107     d ds1P          e ds                  inz
035300070306     d ds2A          e ds                  inz
035400061107     d ds3A          e ds                  inz
035500061107     d ds5E          e ds                  inz
035600061107     d dsCC          e ds
035700061107     d dsCV          e ds                  inz
035800061107     d dsEI          e ds                  inz
035900061107     d dsTB          e ds                  inz
036000061107      *
036100030515     d TIBS02ds      e ds                  inz
036200040607     d   T02mod      e                     inz('C')
036300030805     d dGED          e ds
036400130206     d dILV          e ds
036500061031     d dVPOdeco      e ds                  inz
036600031124     d dAR5gen       e ds                  inz
036700061030     d dAR5bnb       e ds                  inz
036800170517     d dCLI          e ds                  inz
036900080724
037000131210       // -?Flag operativi di FNEVB - campo EVBNOT?
037100131206     d dEVB01        e ds                  inz     qualified
037200101220       // -?Flag operativi di FNORM - campo ORMFLO?
037300100517     d dORM01        e ds                  inz
037400101220       // -?Parametri per TRUL21R (calcolo limiti importo c/assegno)?
037500030515     d TRUL21ds      e ds                  inz
037600120702     d   I21tla      e                     inz('L')
037700101220       // -?Parametri per FNLV55R (reperimento terminal arr/par/£6)?
037800070212     d FNLV55ds      e ds                  inz
037900120702     d   D55tpt      e                     inz('6')
038000101220       // -?Parametri per FNLV80R (decodifica consegna richiesta)?
038100051129     d FNLV80ds      e ds                  inz
038200120702     d   ILV80tla    e                     inz('C')
038300101220       // -?Parametri per TNSD99R (calcolo tempi di delivery)?
038400090619     d TNSD99ds      e ds                  inz
038500120702     d   D98tla      e                     inz('C')
038600130227
038700130227       // -?Tab. "MRA" = Bart-Mailing?
038800130227     d dMRAdan       e ds                  inz
038900130227     d   §MRAddes    e                     inz('PDA-Oltre 100 log "01"+
039000130227     d                                             ')
039100130227     d   §MRAdreg    e                     inz('COR')
039200130227     d   §MRAdmitt   e                     inz(c_Mittente)
039300130227     d   §MRAddest   e                     inz('stefano.merighi+
039400130227     d                                          @brt.it;')
039500130227     d   §MRAdidpro  e                     inz('1')
039600130227     d   §MRAdoutqi  e                     inz('EMAILIN')
039700130227
039800130227       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
039900130227     d TRTCM1ds      e ds                  inz
040000130716         //?·§CM1mitt = Indirizzo e-mail del mittente?
040100130227     d   §CM1mitt    e                     inz(c_Mittente)
040200130716         //?·§CM1dst  = Indirizzo e-mail del destinatario?
040300130227     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
040400130716         //?·§CM1tips = Tipo lettera via e-mail:?
040500130716         // ?"LET" = testo allegato in corpo con logo?
040600130716         //         ?(richiede righe libere iniziali per il logo)?
040700130716         // ?"COR" = testo integrato senza logo?
040800130716         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
040900130716         // ?"SP1" = spool allegato (in formato PDF)?
041000130227     d   §CM1tips    e                     inz('COR')
041100130716         //?·§CM1po   = Sede?
041200130227     d   §CM1po      e                     inz(c_Sede)
041300130716         //?·§CM1var  = Oggetto e-mail?
041400130227     d   §CM1var     e                     inz('*OBJM*+
041500130227     d                                     Sped. elaborata oltre 100 +
041600130227     d                                     volte in FIILV - vedi log +
041700130227     d                                     "01"')
041800130716         //?·§CM1sts  = Stato?
041900130227     d   §CM1sts     e                     inz(*off)
042000130716         //?·§CM1idp  = Id processo?
042100130227     d   §CM1idp     e                     inz('2')
042200101220
042300101220       // -?Status ds?
042400030515     d Status         sds
042500120702     d   SDSpgm          *proc
042600120702     d   SDSprm          *parms
042700120702     d   SDSusr              254    263
042800101220
042900101220       // -?Parametro Numero Distinta?
043000101220     d ds_Distinta     ds             9    inz  qualified
043100101220     d   P_Fgs                        3s 0 inz
043200101220     d   P_Ndc                        6s 0 inz
043300080430
043400101207       // -?Parametro Numero Spedizione?
043500110207     d ds_Spedizione   ds            16    inz  qualified
043600080430     d   P_Aas                        4s 0 inz
043700080430     d   P_Lnp                        3s 0 inz
043800080430     d   P_Nrs                        2s 0 inz
043900080430     d   P_Nsp                        7s 0 inz
044000101220
044100080519     d BARcodeDS       ds            17    inz
044200120702     d   BARcdeAAS                    2s 0 inz
044300120702     d   BARcdeLNP                    3s 0 inz
044400120702     d   BARcdeNRS                    2s 0 inz
044500120702     d   BARcdeNSP                    7s 0 inz
044600101220
044700030515     d KEYpar          ds             8    inz
044800120702     d   WTPpar                1      1    inz
044900120702     d   WCDpar                2      3    inz
045000120702     d   Wprogr                4      8    inz('S0001')
045100101220
045200140303     d Elem_CP         ds                  inz
045300120702     d   WCPdcm                       8s 0 inz
045400140303     d   WCPocm                       4s 0 inz
045500120702     d   WCPncl                       5s 0 inz
045600140303     d   WCPfir                            inz  like(EVB2dati)
045700101213
045800101213       //--------------------------------------------------------------
045900101213       //?Definizione variabili globali.                               ?
046000101213       //--------------------------------------------------------------
046100101213
046200101220       // -?Parametri?
046300080430     d Spedizione      s             16
046400101217     d Distinta        s              9
046500101217     d DataOraCrea     s             14
046600080430     d Risultato       s              2
046700080715     d RootFolder      s            100
046800080519     d FileName        s             30
046900080731     d TemplateName    s             20
047000110201     d LastRow         s            120
047100131016     d RtnEsito        s             10i 0                                      // -?NON usato?
047200131016     d RtnOpCode       s             10                                         // -?NON usato?
047300131016     d RtnStatus       s             10i 0                                      // =?SQLcode?
047400080430     d P_Prt           s              1
047500101220
047600130214       // -?Flags booleani?
047700101220     d $Mamma          s               n   inz
047800130215     d $Mamma_FedEx    s               n   inz
047900101220     d $FIAR8          s               n   inz
048000130411     d $§PDF           s               n   inz
048100110204     d $FIILV          s               n   inz
048200101220     d $NoDesPartic    s               n   inz
048300110117     d*// $BolloVirt      s               n   inz(*off)
048400120113     d $No_x_Cons      s               n   inz
048500101220     d $ConsParz       s               n   inz
048600101220     d $CAeur          s               n   inz
048700100517     d $PreavvORM      s               n   inz
048800101220     d $GeoAtt         s               n   inz
048900131206     d $1o_Passaggio   s               n   inz
049000101220
049100101220       // -?Indici di schiera?
049200080523     d XX              s              3  0 inz
049300140304     d YY              s              3  0 inz
049400080523     d WX              s              3  0 inz
049500130313
049600130313       // -?Nome esteso Libreria/File del file TITAS30C?
049700130313     d wLibFileTITAS   s             21a   inz
049800080724
049900101207       // -?Campi di stampa?
050000130227     d P_Fgs2          s                   inz  like(ds_Distinta.P_Fgs)
050100130227     d P_Ndc2          s                   inz  like(ds_Distinta.P_Ndc)
050200101220
050300101220       // -?Campi di comodo alfanumerici?
050400030717     d w_CoOr          s             35    inz
050500081030     d wAWB            s             12    inz
050600030515     d w001a           s              1    inz
050700030515     d w008a           s              8    inz
050800030515     d w015a           s             15    inz
050900100517     d w053a           s             53    inz
051000070424     d wDateA          s              8    inz
051100070717     d wGeoTel         s              1    inz
051200070717     d wGeoCon         s              1    inz
051300080718     d wSysName        s              8    inz
051400130411      *? x TEST ?
051500130411     d $Test           s              3    inz
051600101220
051700101220       // -?Campi di comodo numerici?
051800030515     d w0080           s              8  0 inz
051900030624     d w0112           s             11  2 inz
052000030515     d w0140           s             14  0 inz
052100071105     d wDate           s              8  0 inz
052200090120     d w_Row           s              3  0 inz(c_Row)
052300110309     d wCountRec01     s              5  0 inz
052400101220
052500131008       // -?Campi utilizzati per impostazione dell'output?
052600110117     d*// wBolloVirt      s              4  2 inz
052700030515     d Wcas            s             13  3 inz
052800030515     d Wiva            s             11  2 inz
052900071015     d Wpdc4A          s              4    inz
053000030515     d Data_EUR        s               d   datfmt(*eur)   inz(*loval)
053100090619     d Data_DMY        s               d   datfmt(*dmy)   inz(*loval)
053200101220
053300101220       // -?Campi utilizzati per il salvataggio momentaneo di dati?
053400040311     d SAVlna          s                   like(DB0lna)   inz(*loval)
053500121018     d wB0cfi          s                   like(DB0cpi)   inz
053600121018     d wB0piv          s                   like(DB0cpi)   inz
053700120702     d wB0nrs          s                   like(ARBnrs)   inz
053800070514     d wB0ntc          s                   like(ARBntc)   inz
053900080523     d wB0dcm          s                   like(ARBdcm)   inz
054000070514     d wB0dcp          s                   like(ARBdcp)   inz
054100081120     d wB0tmc          s                   like(ARBtmc)   inz
054200130205     d wB0fiv          s                   like(AR6fiv)   inz
054300080506     d Wnsc            s                   like(ARTnsc)   inz
054400080506     d Wtrc            s                   like(AR4trc)   inz
054500030515     d Wctr            s                   like(DB0ctr)   inz
054600031124     d Wdsp            s                   like(DB0dft)   inz
054700090619     d WddtCLN         s                   like(DB0drt)   inz
054800090619     d WdspCLN         s              6  0                inz
054900030515     d Wsvx            s                   like(AR6sv1)   inz
055000030515     d Wvax            s                   like(AR6va1)   inz
055100110203     d wCod_Log        s                   like(ILVcol)   inz
055200090619
055300140303       //--------------------------------------------------------------
055400140303       //?Definizione procedure usate.                                 ?
055500090619       //--------------------------------------------------------------
055600100322
055700100322       // -?Procedure name: getCurrentSystemName?
055800100322       //  ?Purpose:        Reperisce l'attributo di rete current System Name?
055900100322       //  ?Returns:        Current System Name?
056000130716       //  ?(vedi "QWCRNETA")?
056100100322     d getCurrentSystemName...
056200100322     d                 pr             8a
056300100322
056400100322       //--------------------------------------------------------------
056500140303       //?Definizione procedure esterne.                               ?
056600100322       //--------------------------------------------------------------
056700100322
056800110201       // -?Calcolo check digit del barcode?
056900110201     d TRUL28ds      e ds                  inz
057000110201     d  I28tla       e                     inz('L')
057100110201     d  I28mod       e                     inz('BAR')
057200110201     d TrUl28r1        pr                  extpgm('TRUL28R1')
057300110201     d  trul28ds                           likeds(TRUL28ds)
057400110201
057500100322       // -?Calcolo tempi di delivery?
057600100322      /copy gaitrasrc/srcProtoPR,TNSD99R
057700130206
057800130206       // -?Ricerca/Controllo tabelle?
057900130206      /copy gaitrasrc/srcProtoPR,TIBS02R
058000100322
058100110201       // -?Parametri per API "QWCRNETA"?
058200110201      /copy QSYSINC/QRPGLESRC,QWCRNETA
058300110201     d qWcrNetA_dataReturned...
058400110201     d                 ds                  qualified inz
058500110201     d  qWcRDR                             likeds(qWCRDR)
058600110201     d  qWcOAT                       10i 0 dim(00001)
058700110201     d  qWcARRAY                     24
058800110201     d qWCRNETA_networkAttributeInformationTable...
058900110201     d                 ds                  qualified inz
059000110201     d  qWcRAT                             likeds(qWCRAT)
059100110201     d  qWcDATA                       8
059200100322       // -?Reperimento attributo di rete current System Name?
059300100322       //  ?(vedi "getCurrentSystemName")?
059400100322      /copy gaitrasrc/srcProtoPR,QWCRNETA
059500130227
059600130227       // -?Parametri API QCAPCMD (Process Commands)?
059700130227     d Qcmd            s           2048    inz  varying
059800130227      /copy qSysInc/qRpgleSrc,QCAPCMD
059900130227       // -?API QCAPCMD (Process Commands)?
060000130227      /copy gaitrasrc/srcProtoPR,QCAPCMD
060100130227
060200130227       // -?Parametri gestione errori API.?
060300130227      /copy qsysinc/qRpgleSrc,QUSEC
060400080710
060500080710       //--------------------------------------------------------------
060600140303       //?Definizione key-list.                                        ?
060700080710       //--------------------------------------------------------------
060800130313
060900130422       // -?File FIIRD11L?
061000130422     d k01fiird11    e ds                  extname(FIIRD11L : *key)
061100130313     d                                     prefix(k_)
061200130313     d                                     inz
061300130313
061400130313       // -?File TITAS30C?
061500130313     d k05titas30    e ds                  extname(TITAS30C : *key)
061600130313     d                                     prefix(k_)
061700130313     d                                     inz
061800131206
061900131206       // -?File FNEVB04L?
062000131206     d keyFNEVB04    e ds                  extname(FNEVB04L : *key)
062100131206     d                                     prefix(k_)
062200131206     d                                     inz
062300140303
062400140303       // -?File FNEVB22L?
062500140303     d keyFNEVB22    e ds                  extname(FNEVB22L : *key)
062600140303     d                                     prefix(k_)
062700140303     d                                     inz
062800170517
062900170517       // -?File TFNTC01L?
063000170517     d keyTFNTC01    e ds                  extname(TFNTC01L : *key)
063100170517     d                                     prefix(k_)
063200170517     d                                     inz
063300080710
063400101207       // -?File FIDST01L?
063500110228     d k03fidst01    e ds                  extname(FIDST01L : *key)
063600080710     d                                     prefix(k_)
063700080710     d                                     inz
063800090216
063900110202       // -?File FIILV01L?
064000110202     d k06fiilv01    e ds                  extname(FIILV01L : *key)
064100110207     d                                     prefix(k_)
064200090216     d                                     inz
064300030515
064400101220       //==============================================================
064500101220       //?RIEPILOGO INDICATORI                                         ?
064600101220       //--------------------------------------------------------------
064700101220       // 31    - di comodo
064800101220       // 35    - Porto Franco: segnala
064900101220       //         "PER RICEVUTA DELLA MERCE SENZA RISERVE"
065000101220       //?36   ?- Richiesta FIRMA per QUIETANZA in 2ª copia
065100101220       //         e indica "TOTALE DA ESIGERE AL RITIRO"
065200120524       // 70    - Trattasi di BOLLA BRT
065300101220       // 71    - Trattasi di BOLLA DPD
065400101220       // 72    - Trattasi di BOLLA FedEx
065500101220       // 73    - Trattasi di BOLLA EuroExpress
065600101220       //==============================================================
065700030515
065800101213       //--------------------------------------------------------------
065900101213       //?M A I N - L I N E                                            ?
066000101213       //--------------------------------------------------------------
066100101213
066200030515     c     *Entry        plist
066300080430     c                   parm                    Spedizione
066400101217     c                   parm                    Distinta
066500101217     c                   parm                    DataOraCrea
066600080430     c                   parm                    Risultato
066700080715     c                   parm                    RootFolder
066800080519     c                   parm                    FileName
066900080731     c                   parm                    TemplateName
067000110201     c                   parm                    LastRow
067100080430     c                   parm                    RtnEsito
067200080430     c                   parm                    RtnOpCode
067300080430     c                   parm                    RtnStatus
067400080430     c                   parm                    P_Prt
067500080430      *
067600110519      *?P_Prt:?non passato se richiamato da server, serve solo se?
067700110519      *       ?chiamato da menù AS400 x test?
067800101221      *   Valori: qualsiasi = richiamato da pgm. per test (stampa)
067900080430      *
068000070123      *
068100130924      * Operazioni iniziali
068200131016       //?SE rilevati errori nei parametri di input =>?
068300131016       //?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
068400070123     c                   exsr      sr_Start
068500030910      *
068600130924      * Pulizia dei campi di output e/o di lavoro
068700130924     c                   exsr      sr_ClrFLD
068800030515      *
068900130924      * Caricamento dei dati per l'immagine della LdV
069000131016       //?SE rileva errori nella distinta o nella spedizione =>?
069100131016       //?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
069200110208     c                   exsr      sr_CarARB
069300030515      *
069400130924      * Controllo del tipo LDV di cui generare l'immagine
069500110208     c                   exsr      sr_TipoLDV
069600030515      *
069700080430      * Impostazione dei campi di output con quelli della DS
069800110208     c                   exsr      sr_PrepPRT
069900030515      *
070000080430      * Decodifica dei dati della LDV
070100110208     c                   exsr      sr_DecodLDV
070200080806      *
070300080806      * Impostazione dati per immagine LdV
070400131016     c                   exsr      sr_DatiOUT
070500030515      *
070600030515      * Chiusura pgm
070700080430     c                   exsr      sr_End
070800030515      *
070900030515      *---------------------------------------------------------------*
071000050217      *?OPERAZIONI INIZIALI                                          ?*
071100030515      *---------------------------------------------------------------*
071200030515     c     *InzSR        BEGSR
071300070424      *
071400070424      * Reperisco la data del giorno
071500070424     c                   time                    w0140
071600070424     c                   move      w0140         w0080
071700070424     c     *eur          movel     w0080         Data_EUR
071800070424     c     *iso          movel     Data_EUR      w0080
071900070424     c                   move      w0080         wDateA
072000071105     c                   eval      wDate  = w0080
072100030515      *
072200030515      * Imposto i campi chiave "fissi"
072300040223     c                   eval      TBLkut = 1
072400100901     c                   eval      ARStrc = 'C'
072500030515      *
072600040617      * Impostazione dati in KPJBA
072700070308     c                   eval      KNMUS  = SDSusr
072800030515      *
072900030515      * Reperisco la moneta di conto
073000030515     c                   clear                   dGED
073100040607     c                   clear                   TIBS02ds
073200040607     c                   eval      T02mod = 'C'
073300040607     c                   eval      T02cod = 'GED'
073400040607     c                   eval      T02ke1 = '1         '
073500030515     c                   call      'TIBS02R'
073600030515     c                   parm                    KPJBA
073700040607     c                   parm                    TIBS02ds
073800030515if  1c                   if        T02err = *blanks
073900030515     c                   movel     T02uni        dGED
074000030515e   1c                   endif
074100061031      *
074200131008      * Reperisco data di decorrenza indicaz. "Per ricevuta di ...
074300131008      *                                        senza riserve"
074400131008      *              e di decorrenza indicaz. "Consegna al mattino"
074500061031     c                   clear                   dVPOdeco
074600061031     c                   clear                   TIBS02ds
074700061031     c                   eval      T02mod = 'C'
074800071112     c                   eval      T02cod = 'VPO'
074900061031     c                   eval      T02ke1 = 'DECO      '
075000061031     c                   call      'TIBS02R'
075100061031     c                   parm                    KPJBA
075200061031     c                   parm                    TIBS02ds
075300061031if  1c                   if        T02err = *blanks
075400061031     c                   movel     T02uni        dVPOdeco
075500061031e   1c                   endif
075600030515      *
075700030515     c                   ENDSR
075800070123      *
075900101220      /free
076000101220
076100101220       //--------------------------------------------------------------
076200101220       //?ALTRE OPERAZIONI INIZIALI (NON PIÙ IN *INZSR)                ?
076300101220       //--------------------------------------------------------------
076400101220       BEGSR  sr_Start;
076500101220
076600101220         exec sql  set option  dynusrprf = *owner,
076700101220                               closqlcsr = *endmod;
076800101220
076900101221         *inLR = *on;
077000080509
077100110517         // -?Impostazione iniziale dei dati di ritorno?
077200080509         Risultato = 'OK';
077300110202         clear  RootFolder;
077400110202         clear  FileName;
077500120524         //TemplateName = 'ldvbase00 ';   //?Standard - BARTOLINI senza box aggiuntivo?
077600120524         //TemplateName = 'ldvbase10 ';   //?Standard - BARTOLINI-BRT senza box aggiuntivo?
077700130924         TemplateName = 'ldvbase20 ';     //?Standard - BRT senza box aggiuntivo?
077800110202         clear  LastRow;
077900110202         clear  RtnEsito;
078000110202         clear  RtnOpCode;
078100110202         clear  RtnStatus;
078200110207
078300110207         // -?Apertura archivio di output - comunque utilizzato?
078400110207         if  NOT  %open(FIILV01L);
078500110207           open(e)  FIILV01L;
078600110207           if  %error;
078700110207             exsr  sr_ErrOpenFile;
078800110207           endif;
078900110207         endif;
079000110207
079100110202         clear  k06fiilv01;
079200110203         clear  FIILV000;
079300080509
079400110203         // »? STEP 0 ?«
079500131016         // -?Controllo parametri ricevuti.  SE rileva errori =>?
079600131016         //  ?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
079700130716         exsr  sr_STEP_0_CtrlParms;
079800110203
079900080430
080000101213         // -?Impostazione spedizione ricevuta come parametro?
080100080430         ds_Spedizione = Spedizione;
080200110126         // -?Impostazione distinta ricevuta come parametro?
080300110126         ds_Distinta = Distinta;
080400080430
080500101213         // -?Impostazione iniziale dei dati nella ds FNLSB5DS?
080600130924         //clear FNLSB5ds;            ?(già così)?
080700080430         DB0tbo = 'A';
080800110202         DB0aas = ds_Spedizione.P_Aas;
080900110202         DB0lnp = ds_Spedizione.P_Lnp;
081000110202         DB0nrs = ds_Spedizione.P_Nrs;
081100110202         DB0nsp = ds_Spedizione.P_Nsp;
081200080519
081300101213         // -?Impostazione campo di output "identificativo spedizione"?
081400080519         clear  BarCodeDS;
081500080519         BARcdeAAS = %int( %subst( %editc( DB0aas : 'X' ) : 3 : 2) );
081600080519         BARcdeLNP = DB0lnp;
081700080519         BARcdeNRS = DB0nrs;
081800080519         BARcdeNSP = DB0nsp;
081900080519         FileName  = BarCodeDS;
082000130313
082100130716         // -?Impostazione chiave di accesso al file FIIRD11L?
082200130313         k_IRDfgsndc = Distinta;
082300130313
082400130313         // -?Impostazione chiave di accesso al file TITAS30C?
082500130313         clear  k05titas30;
082600130313         k_TASaas = DB0aas;
082700130313         k_TASlnp = DB0lnp;
082800130313         k_TASnrs = DB0nrs;
082900130313         k_TASnsp = DB0nsp;
083000131206
083100131206         // -?Impostazione chiave di accesso al file FNEVB04L?
083200131206         clear  keyFNEVB04;
083300131206         k_EVBaas = DB0aas;
083400131206         k_EVBlnp = DB0lnp;
083500131206         k_EVBnrs = DB0nrs;
083600131206         k_EVBnsp = DB0nsp;
083700131210         //k_EVBcev = 'MIC';          ?(leggere TUTTI i rec.)?
083800140303
083900140303         // -?Impostazione chiave di accesso al file FNEVB22L?
084000140303         clear  keyFNEVB22;
084100140303         k_EVB2aas = DB0aas;
084200140303         k_EVB2lnp = DB0lnp;
084300140303         k_EVB2nrs = DB0nrs;
084400140303         k_EVB2nsp = DB0nsp;
084500140303         //k_EVB2dtv = .....;
084600140303         //k_EVB2orv = .....;
084700140303         //k_EVB2cev = 'P  ';
084800110204
084900110204         // -?Impostazione chiave di accesso al file FIILV01L?
085000110204         k_ILVaas = DB0aas;
085100110204         k_ILVlnp = DB0lnp;
085200110204         k_ILVnrs = DB0nrs;
085300110204         k_ILVnsp = DB0nsp;
085400110225         k_ILVfgs = ds_Distinta.p_Fgs;
085500110225         k_ILVndc = ds_Distinta.p_Ndc;
085600120702         clear  dILVlibero;
085700130411
085800130411
085900130411         IF  '1' <> '1';
086000130411
086100130924           // -? CHIODI MOMENTANEI PER TEST                                 ?
086200130924           //  ? da attivare per ricavare immagini di esempio forzando sped ?
086300130924           clear  $Test;
086400130411           Select;
086500130411             // -?Spedizione FedEx?
086600130411             When  Spedizione = '2011050001843581' and Distinta = '145040335';
086700130411               $Test = 'FED';
086800130411             // -?Spedizione DPD?
086900130411             When  Spedizione = '2011190000579210' and Distinta = '145040339';
087000130411               $Test = 'DPD';
087100130411             // -?Spedizione con Consegna Parziale Precedente?
087200130411             When  Spedizione = '2011001000816011' and Distinta = '145040361';
087300130411               $Test = 'C_P';
087400130411             // -?Spedizione Normale?
087500130411             When  Spedizione = '2011163000042281' and Distinta = '145040333';
087600130411               $Test = 'NOR';
087700130411             // -?Altra spedizione: NON ELABORABILE?
087800130411             Other;
087900130411               Risultato = 'NO';
088000130924               //RtnOpCode = 'NO_IN_TEST';
088100130924               //RtnEsito  = -1;
088200130924               // -?Uscita dalla s.p.?
088300130411               exsr  sr_End;
088400130411           EndSl;
088500130411
088600130411         ENDIF;
088700130411
088800080430
088900110208         // -?Apertura archivi di input - comunque utilizzati?
089000110207         if  NOT  %open(TABEL00F);
089100110207           open(e)  TABEL00F;
089200110207           if  %error;
089300110207             exsr  sr_ErrOpenFile;
089400110207           endif;
089500110207         endif;
089600110207
089700110207         if  NOT  %open(AZORG01L);
089800110207           open(e)  AZORG01L;
089900110207           if  %error;
090000110207             exsr  sr_ErrOpenFile;
090100110207           endif;
090200110207         endif;
090300110207
090400110207         if  NOT  %open(FNARB01L);
090500110207           open(e)  FNARB01L;
090600110207           if  %error;
090700110207             exsr  sr_ErrOpenFile;
090800110207           endif;
090900110207         endif;
091000110207         if  NOT  %open(FIAR601L);
091100110207           open(e)  FIAR601L;
091200110207           if  %error;
091300110207             exsr  sr_ErrOpenFile;
091400110207           endif;
091500110207         endif;
091600110207         if  NOT  %open(FIAR701L);
091700110207           open(e)  FIAR701L;
091800110207           if  %error;
091900110207             exsr  sr_ErrOpenFile;
092000110207           endif;
092100110207         endif;
092200110207         if  NOT  %open(FIAR901L);
092300110207           open(e)  FIAR901L;
092400110207           if  %error;
092500110207             exsr  sr_ErrOpenFile;
092600110207           endif;
092700110207         endif;
092800110207         if  NOT  %open(FIDST01L);
092900110207           open(e)  FIDST01L;
093000110207           if  %error;
093100110207             exsr  sr_ErrOpenFile;
093200110207           endif;
093300110207         endif;
093400101213
093500101213         if  NOT  %open(FIAR401L);
093600101213           open(e)  FIAR401L;
093700101213           if  %error;
093800101213             exsr  sr_ErrOpenFile;
093900101213           endif;
094000101213         endif;
094100101213         if  NOT  %open(FIAR501L);
094200101213           open(e)  FIAR501L;
094300101213           if  %error;
094400101213             exsr  sr_ErrOpenFile;
094500130411           endif;
094600130411         endif;
094700101213
094800101220       ENDSR;
094900101220
095000101213       //--------------------------------------------------------------
095100101213       //?Restituzione errore in fase di apertura di un file           ?
095200101213       //--------------------------------------------------------------
095300101213       BEGSR  sr_ErrOpenFile;
095400101213
095500101213         Risultato = 'NO';
095600130924         //RtnOpCode = 'ERR_OPEN  ';
095700101213         //RtnEsito  = -1;
095800101213
095900130924         // -?Uscita dalla s.p.?
096000101213         exsr  sr_End;
096100101213
096200101213       ENDSR;
096300130716
096400130716       //--------------------------------------------------------------
096500130716       //?»  STEP 0  «                                                 ?
096600130716       //?Controllo dei parametri ricevuti, reperimento nome del       ?
096700130716       //?sistema e dei percorsi per le immagini in base ad esso.      ?
096800130716       //--------------------------------------------------------------
096900130716       BEGSR  sr_STEP_0_CtrlParms;
097000130716
097100130716         //   _________________________________________________________
097200130716         // -? Controllo lunghezza e numericità dei parametri ricevuti ?
097300130716         //   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
097400130716         SELECT;
097500130716
097600130716           // -?NUMERO SPEDIZIONE?
097700130716           WHEN  ( %len( %trimr( Spedizione ) ) )  <> 16     or
097800130716                 ( %check (c_Digits : Spedizione) ) > *zero  or
097900130716                                      Spedizione   <= *zero;
098000130716             Risultato = 'XX';
098100130924             //RtnOpCode = 'ERR_NUMSPE';
098200130716             //RtnEsito  = -1;
098300130716             // -?Registrazione log in FIILV?
098400130716             wCod_Log = '21';
098500130716             exsr  sr_WrtFIILV;
098600130716             // -?Uscita dalla s.p.?
098700130716             exsr  sr_End;
098800130716
098900130716           // -?NUMERO DISTINTA?
099000130716           WHEN  ( %len( %trimr( Distinta ) ) )  <> 9      or
099100130716                 ( %check (c_Digits : Distinta) ) > *zero  or
099200130716                                      Distinta   <= *zero;
099300130716             Risultato = 'XX';
099400130924             //RtnOpCode = 'ERR_DISTIN';
099500130716             //RtnEsito  = -1;
099600130716             // -?Registrazione log in FIILV?
099700130716             wCod_Log = '21';
099800130716             exsr  sr_WrtFIILV;
099900130716             // -?Uscita dalla s.p.?
100000130716             exsr  sr_End;
100100130716         ENDSL;
100200130716
100300130716         // ________________________________________________________
100400130716         //? Reperimento nome del sistema per impostazione percorso ?
100500130716         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
100600130716         wSysName = getCurrentSystemName();
100700130716         if  wSysName = *blank;
100800130716           Risultato  = 'NO';
100900130924           //RtnOpCode  = 'SYS_ERR   ';
101000130924           //RtnEsito   = -1;
101100130924           // -?Uscita dalla s.p.?
101200130716           exsr  sr_End;
101300130716         endif;
101400130716
101500130716         // ______________________________________________________
101600130716         //? Reperimento dei percorsi in base al nome del sistema ?
101700130716         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
101800130716         clear  dILV;
101900130716         reset  TIBS02ds;
102000170707         T02cod = 'ILG';
102100130716         T02ke1 = %subst( wSysName : 1 : 6 );
102200130716         T02ke2 = 'LINEA';
102300130716         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
102400130716         if  T02err = *blank;
102500130716           dILV = T02uni;
102600130716         endif;
102700130716
102800130716       ENDSR;
102900110208
103000110208       //--------------------------------------------------------------
103100130924       //?Pulizia dei campi di output e/o di lavoro                    ?
103200110208       //--------------------------------------------------------------
103300130924       BEGSR  sr_ClrFLD;
103400080430
103500081021         clear ds_01;
103600101213       //clear ds_02;                      ?(riga vuota)?
103700080430         clear ds_03;
103800080430         clear ds_04;
103900080430         clear ds_05;
104000101213       //clear ds_06;                      ?(riga vuota)?
104100080430         clear ds_07;
104200101213       //clear ds_08;                      ?(riga vuota)?
104300080430         clear ds_09;
104400080430         clear ds_10;
104500080430         clear ds_11;
104600080430         clear ds_12;
104700080430         clear ds_13;
104800080430         clear ds_14;
104900130205         clear ds_140;
105000080430         clear ds_15;
105100080430         clear ds_16;
105200080430         clear ds_17;
105300080430         clear ds_18;
105400130205         clear ds_180;
105500080430         clear ds_19;
105600080430         clear ds_20;
105700080430         clear ds_21;
105800080430         clear ds_22;
105900080430         clear ds_23;
106000080430         clear ds_24;
106100080430         clear ds_25;
106200101213       //clear ds_26;                      ?(vuoto)?
106300080430         clear ds_27;
106400080430         clear ds_28;
106500081021         clear ds_29;
106600081021         clear ds_30;
106700110215         clear ds_31;
106800081021         clear ds_32;
106900081021         clear ds_33;
107000081021         clear ds_34;
107100081023         clear ds_37;
107200081030         clear ds_Fed;
107300080430
107400080430      /end-free
107500030515      *
107600060524     c                   clear                   dsBL4A
107700061030     c                   clear                   dsBL4I
107800110204     c                   clear                   dsBL4M
107900100517     c                   clear                   dOrm01
108000041207      *
108100070514     c                   reset                   $Mamma
108200130215     c                   reset                   $Mamma_FedEx
108300060921     c                   reset                   $FIAR8
108400130411     c                   reset                   $§PDF
108500110204     c                   reset                   $FIILV
108600030710     c                   reset                   $NoDesPartic
108700110117     c*//                reset                   $BolloVirt
108800040707     c                   reset                   $No_x_Cons
108900070319     c                   reset                   $ConsParz
109000030515     c                   reset                   $CAeur
109100100901     c                   reset                   $PreavvORM
109200101220     c                   reset                   $GeoAtt
109300070717     c                   clear                   wGeoTel
109400070717     c                   clear                   wGeoCon
109500070717     c                   clear                   Wdsp
109600090619     c                   clear                   WddtCLN
109700090619     c                   clear                   WdspCLN
109800030717     c                   clear                   W_CoOr
109900110117     c*//                clear                   wBolloVirt
110000030515     c                   clear                   Wcas
110100030515     c                   clear                   Wiva
110200071015     c                   clear                   Wpdc4a
110300121018     c                   clear                   wB0cfi
110400121018     c                   clear                   wB0piv
110500031015     c                   clear                   wB0nrs
110600070514     c                   clear                   wB0ntc
110700080523     c                   clear                   wB0dcm
110800070514     c                   clear                   wB0dcp
110900081120     c                   clear                   wB0tmc
111000130205     c                   clear                   wB0fiv
111100030515      *
111200030515     c                   clear                   $NP
111300070319     c                   reset                   $CP
111400070319     c                   reset                   $CPdcm
111500030515      *
111600030515     c                   movea     *zeros        *in(01)
111700030515      *
111800030515     c                   ENDSR
111900030515      *
112000030515      *---------------------------------------------------------------*
112100131008      *?Determino il modulo LDV da usare per l'immagine              ?*
112200131008      *?(se DPD, FedEx o BRT - con consegne parziali precedenti o    ?*
112300131008      *? promrmoria per l'autotrasportatore).                        ?*
112400030515      *---------------------------------------------------------------*
112500110208     c     sr_TipoLDV    BEGSR
112600070215      *
112700070212      * Impostazione della data spedizione
112800110208      * (se non già impostata in subr. "sr_CarARB")
112900070212if  1c                   if        Wdsp   =  *zeros
113000070212if  2c                   if        DB0mgs <> *zeros
113100070212     c                   eval      Wdsp   =  (DB0aas * 10000) + DB0mgs
113200070212x   2c                   else
113300070212      *   prendo la data del giorno se non c'è la data spedizione
113400070212     c                   time                    w0140
113500070212     c                   move      w0140         w0080
113600070212     c     *eur          movel     w0080         Data_EUR
113700070212     c     *iso          movel     Data_EUR      Wdsp
113800070212e   2c                   endif
113900070212e   1c                   endif
114000040607      *
114100070308      * Dati della filiale del mittente
114200040607     c                   eval      WX     =  DB0ccm / 10000
114300040607if  1c                   if        WX     <> ORGfil
114400040607     c     WX            chain     AZORG
114500040607e   1c                   endif
114600040607if  1c                   if        %found(AZORG01L)
114700040607     c                   movel     ORGde3        OG143ccm
114800040607x   1c                   else
114900040607     c                   clear                   OG143ccm
115000040607e   1c                   endif
115100040311      *
115200040311      * Dati della linea di arrivo
115300131203      *?(SONO GIÀ STATI REPERITI NELLA SUBR. "SR_DECLNA",?
115400131203      *? ESEGUITA DALLA SUBR. "SR_CARARB")?
115500070212      *
115600070212      * Dati della capofila della linea di arrivo
115700131205      * - per verificare TELLUS
115800070717      *
115900070717      *** Imposta flag attivazione GEO x LNA
116000070717     c                   eval      wGeoCon = A§OGtlls
116100070717     c                   eval      wGeoTel = A§OGgeot
116200070717if  1c                   if             A§OGdda  > *zeros
116300070717     c                             and  A§OGdda  < C_DateMax_A
116400070717     c                             and  A§OGdda <= wDateA
116500101220     c                   eval      $GeoAtt = *on
116600070717e   1c                   endif
116700070717      *
116800110203if  1c                   IF             wGeoCon <> 'S'
116900070717     c                              or  wGeoTel <> 'S'
117000110203     c                              or  $GeoAtt <> *on
117100070212      * - Reperimento capofila (£6)
117200070212if  2c                   if        DB0lna <> D55lin
117300070212     c                   reset                   FNLV55ds
117400130924     c***?già così:?     eval      D55tpt =  '6'
117500070212     c                   eval      D55lin =  DB0lna
117600070212     c                   eval      D55drf =  Wdsp
117700070212     c                   call      'FNLV55R'
117800070212     c                   parm                    FNLV55ds
117900070212if  3c                   if        D55err <> *blanks
118000070212     c                   eval      D55tfa =  DB0lna
118100070212e   3c                   endif
118200070212e   2c                   endif
118300070321      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
118400070306      *   (flag Tellus del capofila e flag gestione telefonate GEO)
118500070212if  2c                   if        D55tfa <> ORGfil
118600070212     c     D55tfa        chain     AZORG
118700071109if  3c                   if        %found(AZORG01L)
118800070717     c                   movel     ORGde6        OG146cf£6
118900070717     c                   movel     ORGde8        OG148cf£6
119000071109x   3c                   else
119100071109     c                   clear                   OG146cf£6
119200071109     c                   clear                   OG148cf£6
119300071109e   3c                   endif
119400071109e   2c                   endif
119500070717      *** Imposta flag attivazione GEO x capofila £6
119600070717     c                   eval      wGeoCon = £6OGtlls
119700070717     c                   eval      wGeoTel = £6OGgeot
119800071109if  2c                   if             £6OGdda  > *zeros
119900070717     c                             and  £6OGdda  < C_DateMax_A
120000070717     c                             and  £6OGdda <= wDateA
120100101220     c                   eval      $GeoAtt = *on
120200071109e   2c                   endif
120300070717      *
120400070212e   1c                   ENDIF
120500040607      *
120600130411      * Reperimento dati da FIAR5 / "GEN"
120700130411      *   (per verificare se esistono Packing-List e/o PDF allegati
120800130411      *    - oltre che per reperire Nominativo e Telefono)
120900130411     c                   exsr      RepAR5gen
121000130411      *
121100130411      * Verifica se LDV con DDT e/o PDF del mittente
121200130411     c                   exsr      CercaDDToPDF
121300131206      *
121400131206      * Controllo se già effettuato il 1° passaggio c/o destinatario
121500131206     c                   exsr      sr_Check1pass
121600070319      *
121700070319      * Verifica se richiesta spedizione già parzialmente consegnata
121800070319      *   (da stampare nel box "Note per l'autotrasportatore")
121900070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
122000070717      * completamente
122100110203if  1c                   if             wB0dcp   >  *zeros
122200101220     c                             and  $GeoAtt  =  *on
122300070413     c                   eval      $ConsParz     =  *on
122400070319e   1c                   endif
122500030515      *
122600040607      * Tipo LDV (per richiesta firma per ricevuta della merce senza
122700040607      *   riserve)
122800120524      *   (70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)
122900040223     c                   movea     '1000'        *in(70)
123000040311      *
123100030515sel 1c                   SELECT
123200071213      *
123300170517      * LDV DPD?(dalla fil. del mittente, NON della LNP)?
123400071213w   1c                   WHEN      §CMntw = 'DPD'
123500071213     c                   movea     '0100'        *in(70)
123600071213      *
123700071213      * LDV con mittente FedEx
123800150430w   1c                   WHEN      DB0ccm = c_CliFedEx   or
123900150430     c                             DB0ccm = c_CliFedEx2
124000071213     c                   movea     '0010'        *in(70)
124100030515      *
124200030515e   1c                   ENDSL
124300040707      *
124400040707      * Controllo se devo stampare "Non utilizzare per la consegna"
124500040707      * nella copia per il vettore,?senza?copia per il destinatario:
124600130924     c***?(già fatto:)?  reset                   $No_x_Cons
124700071203      * - LdV recapito c/assegni:
124800100322      *   stampata in arrivo e con
124900120113if  1c                   if             DB0lnp =  102
125000070306     c                             and  DB0cbo =  'FW'
125100071203     c                             and  DB0nft =  *zeros
125200071203      *   - fil. arrivo con GEO attivato
125300071203     c                             and (DB0fl1    =  'A'
125400071203     c                              or  wGeoCon   =  'S'
125500071203      *   - fil. arrivo abilitata al picking
125600120113     c                              or  A§OGpck   =  'S')
125700070306     c                   eval      $No_x_Cons  =  *on
125800120113e   1c                   endif
125900030515      *
126000030515     c                   ENDSR
126100040607      *
126200040607      *---------------------------------------------------------------*
126300130411      *?Verifica esistenza dati del DDT allegato da stampare  o      ?*
126400130411      *?               di uno o più PDF allegati da stampare         ?*
126500040607      *---------------------------------------------------------------*
126600130411     c     CercaDDToPDF  BEGSR
126700040607      *
126800130924     c***?(già fatto:)?  reset                   $FIAR8
126900130924     c***?(già fatto:)?  reset                   $§PDF
127000040607      *
127100130924     c***?(già fatto:)?  exsr      RepAR5gen
127200130411      *
127300130411      * -?Verifica esistenza PDF allegato?
127400130411     c                   eval      $§PDF  = (§AR5als = 'S')
127500130411      *
127600130411      * -?Verifica esistenza packing-list?
127700130411     c                   eval      $FIAR8 = (§AR5alx = 'S')
127800040607      *
127900080505     c                   ENDSR
128000131206      *
128100131206      /free
128200131206
128300131206       //--------------------------------------------------------------
128400131206       //?Verifica se già effettuato il 1° passaggio.                  ?
128500131206       //--------------------------------------------------------------
128600131206       BEGSR  sr_Check1pass;
128700131206
128800131206         //reset  $1o_Passaggio       ?(già fatto)?
128900131206
129000131206         // -?Apertura archivio?
129100131206         If  NOT  %open(FNEVB04L);
129200131206           open(e)  FNEVB04L;
129300131206           if  %error;
129400131206             exsr  sr_ErrOpenFile;
129500131206           endif;
129600131206         EndIf;
129700131206
129800131206         // -?Ciclo di lettura per ricercare l'eventuale 1° passaggio?
129900131210         setgt   %kds(keyFNEVB04 : 4)  FNEVB000;
130000131210         readPe  %kds(keyFNEVB04 : 4)  FNEVB000;
130100131206
130200131206         DoW  Not %eof(FNEVB04L)  and  Not $1o_Passaggio;
130300131206
130400131206           dEVB01 = EVBnot;
130500131206           $1o_Passaggio = (dEVB01.§NOTprg = 'P');
130600131206
130700131206           if  Not $1o_Passaggio;
130800131210             readPe  %kds(keyFNEVB04 : 4)  FNEVB000;
130900131206           endif;
131000131206
131100131206         EndDo;
131200131206
131300131206       ENDSR;
131400131206
131500131206      /end-free
131600030515      *
131700030515      *---------------------------------------------------------------*
131800050217      *?Imposta i campi di stampa con quelli della DS                ?*
131900030515      *---------------------------------------------------------------*
132000110208     c     sr_PrepPRT    BEGSR
132100040223      *
132200040223      * Reperimento dati da FIAR5 / "GEN"
132300130924     c***?(già fatto:)?  exsr      RepAR5gen
132400070212     *** *
132500070212     *** * Impostazione Data spedizione
132600110208     *** * (già fatto nella subr. "sr_TipoLdV")
132700030515      *
132800030515      * - Intestazione moduli
132900030515      *
133000030515      * - Sigla operatore video
133100030515     c                   eval      Psop   =  DB0sop
133200081120      /free
133300081120         //
133400130411         // -?Flag "allega documento di trasporto"?
133500081120         select;
133600081120
133700130411           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
133800081120           when  DB0bam =  'S'   and   wB0tmc <> *blank;
133900081120             evalr Pddt =  'DDT sul collo';
134000081120
134100130411           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
134200081120           when  DB0bam =  'S';
134300081120             evalr Pddt =  'DDT';
134400081120
134500130716           // -?L'indicazione di packing-list è stata spostata in PPKL?
134600130716           //// -?Esiste packing-list abbinata?
134700130411           //when  $FIAR8 =  *on   and   $§PDF =  *off;
134800131203           //  evalr Pddt =  'PACKING-LIST';
134900130716           //// -?LdV con PDF allegato?
135000130411           //when  $FIAR8 =  *off  and   $§PDF =  *on;
135100130411           //  evalr Pddt =  'PACKING-LIST PDF';
135200130716           //// -?LdV con packing-list abbinata & PDF allegato?
135300130411           //when  $FIAR8 =  *on   and   $§PDF =  *on;
135400130411           //  evalr Pddt =  'PACKING-LIST PDF';
135500130411
135600130411           // -?LdV senza PDF allegato & senza packing-list abbinata?
135700081120           other;
135800081120             clear Pddt;
135900081120
136000081120         endsl;
136100081120
136200081120      /end-free
136300030515      *
136400030515      * - Mittente
136500170517     c                   if        *in71
136600170517     c                   exsr      sr_Mitt_DPD
136700170517     c                   endif
136800170517      *
136900030515     c                   if        DB0ccm <> *zeros
137000030515     c                   move      DB0ccm        Pccm
137100030515     c                   endif
137200030515     c                   eval      Prsm   =  DB0rsm
137300030515     c                   eval      Pinm   =  DB0inm
137400030515     c                   eval      Pcam   =  DB0cam
137500030515     c                   eval      Plom   =  DB0lom
137600030515     c                   eval      Pprm   =  DB0prm
137700030515     c                   eval      Pnzm   =  DB0nzm
137800030515      *   Mittente originale
137900030515if  1c                   if        DB0rmo <> *blanks
138000080430     c                   eval      Pkmo   =  'Mitt.Originale '
138100030515     c                   eval      Prmo   =  DB0rmo
138200030515e   1c                   endif
138300030515      *
138400070308      * - Filiale di Arrivo
138500030515     c                   move      DB0lna        Plna
138600030515      *
138700030515      * - Destinatario
138800030515if  1c                   if        DB0ccd <> *zeros
138900030515     c                   move      DB0ccd        Pccd
139000030515e   1c                   endif
139100030515     c                   eval      Prsd   =  DB0rsd
139200030515     c                   eval      Prse   =  DB0rd2
139300030515     c                   eval      Pind   =  DB0ind
139400030515     c                   eval      Pcad   =  DB0cad
139500030515     c                   eval      Plod   =  DB0lod
139600030515     c                   eval      Pprd   =  DB0prd
139700030515     c                   eval      Pnzd   =  DB0nzd
139800030515      *
139900030515      * - Codice tariffa
140000030515if  1c                   if        %subst(DB0tb1:1:1) = 'A'
140100030515      *   - nel box del destinatario se porto assegnato
140200030515     c                   move      DB0ctr        Pcta
140300030515x   1c                   else
140400030515      *   - nel box del mittente se porto franco
140500030515     c                   move      DB0ctr        Pctf
140600030515e   1c                   endif
140700030515      *
140800030515      * - Riferimenti mittente
140900080430     c                   evalr     Prmn   =  %char(DB0rmn)
141000030515     c                   eval      Prma   =  DB0rma
141100061107      * se bolla DPD recupero il riferimento da FIAR4 rcd "I"
141200130924     c***?(già fatto:)?  clear                   dsBL4I
141300060626     c                   if        *in71
141400061103     c                   exsr      RepAR4_I
141500060626     c                   endif
141600030515      *
141700030515      * - Nominativo & telefono
141800031124      *   (da FIAR5 / "GEN")
141900130924     c***?(già fatto:)?  exsr      RepAR5gen
142000031124     c                   eval      Pnom   =  §AR5ref
142100031124     c                   eval      Ptel   =  §AR5teld
142200030515      *
142300030515      * - Spedizione
142400030515      *   (numero: non se Rimesso al Mittente)
142500030515if  1c                   if        DB0ris <> 'M'
142600080430     c                   eval      Plnp   =  %editc(DB0lnp : 'X')
142700080430     c                   eval      Pnrs   =  %editc(DB0nrs : 'Z')
142800080430     c                   eval      Pnsp   =  %editc(DB0nsp : 'Z')
142900030515e   1c                   endif
143000030515      * . . Data spedizione
143100031124     c     *iso          movel     Wdsp          Data_EUR
143200080506     c                   eval      PdspA = %editc( %dec(Data_EUR) : 'Y' )
143300030515      *
143400030515      * - Colli: numero, peso, volume, flag tipo-volume
143500080430     c                   eval      PnclA  =  %editc(DB0ncl : 'Z')
143600080911      /free
143700080911           if  NOT *in71;
143800080911      /end-free
143900080430     c                   eval      PpkfA  =  %editc(DB0pkf : '4')
144000080430     c                   eval      PvlfA  =  %editc(DB0vlf : '4')
144100030515if  1c                   if            DB0fvf <> *blanks
144200030515     c                             and DB0ris <> 'M'
144300081021     c                   movel     DB0fvf        Pfvf
144400030515e   1c                   endif
144500080915      /free
144600080915           endif;
144700080915      /end-free
144800030515      *
144900030515      * - Natura merce
145000030515     c                   eval      Pnat   =  DB0nas
145100030515      *
145200030515      * - Servizio
145300100517      /free
145400100517         if  %subst(DB0tb1:1:1) = 'A'   or   DB0tb2 <> *blanks;
145500110117           exsr  sr_CheckORM_PreavvMail;
145600100517         endif;
145700100517      /end-free
145800100517      * . . Descrizione porto
145900130925      *     (NON se presente ORM con preavviso mail)
146000100517     c                   if        Not  $PreavvORM
146100030515     c                   eval      Pds1   =  DB0dcb
146200100517     c                   endif
146300030515      * . . Descrizione tipo servizio
146400030515if  1c                   if        DB0fst =  'S'
146500030515     c                   eval      Pds2   =  DB0dts
146600030515e   1c                   endif
146700030515      * . . Reso / Franco frontiera
146800030515sel 1c                   select
146900030515w   1c                   when      DB0fbr =  'S'
147000030515     c                   clear                   w008a
147100030515     c                   eval      w008a  =  '   RESO '
147200030515     c                   move      w008a         Pds2
147300030515w   1c                   when      DB0fbr =  'F'
147400030515     c                   clear                   w015a
147500030515     c                   eval      w015a  =  '   FR.FRONTIERA'
147600030515     c                   move      w015a         Pds2
147700030515e   1c                   endsl
147800030515      *
147900080430      * - Zona consegna
148000080430      *   (campo alfanumerico)
148100080430     c                   eval      Pznc   =  %editc(DB0znc : 'X')
148200030515      *
148300030515      * - Flag "*" (gestito a parte)
148400040223      *
148500040223      * - Flag "V" = colli di valore
148600040223if  1c                   if        §AR5bva   <> *blank
148700040223     c                   eval      Pbva   =  'V'
148800040223e   1c                   endif
148900030515      *
149000030515      * - Fermo deposito
149100030515if  1c                   if        DB0ffd <> *blanks
149200080430     c                   eval      Pgc2   =  'FERMO DEPOSITO '
149300030515e   1c                   endif
149400030515      *
149500070308      * - Segnacolli: filiale, serie, dal / al
149600030515if  1c                   if        DB0fls <> *zeros
149700080430     c                   eval      Ppos   =  %editc(DB0fls : 'X')
149800030515e   1c                   endif
149900040607      * - - se bolla con mamma considero la serie della mamma
150000070515if  1c                   if        $Mamma =  *on
150100080430     c                   eval      Psrs   =  %editc(wB0nrs : 'Z')
150200031015x   1c                   else
150300080430     c                   eval      Psrs   =  %editc(DB0nrs : 'Z')
150400031015e   1c                   endif
150500030515      * - Se colli non sequenziali: stampo "NON SEQUENZIALI" + Elenco
150600030515      *   Se collo cumulativo ....:   "     SCD "X" Num.Colli
150700030515      *   Se colli sequenziali ...:   "     SCD "/" SCA
150800030515      *   Se collo singolo .......:   "     SCD
150900031015sel 1c                   select
151000030515      * - - non sequenziali
151100030515w   1c                   when      DB0fns =  'S'
151200131008if  2c                   if        DB0ncl <= 2
151300050511     c                   exsr      ListaScNSd
151400131008x   2c                   else
151500081021     c                   eval      Pnsx   =  'NON SEQUENZIALI'
151600131008e   2c                   endif
151700030515      * - - singolo o sequenziali
151800030515      *     (non se rimesso al mittente)
151900030515w   1c                   when      DB0ris <> 'M'
152000080430     c                   eval      Pnsx   =  %editc(DB0ncd : 'Z')
152100030515if  2c                   if            DB0nca <> *zeros
152200030515     c                             and DB0nca <> DB0ncd
152300080430     c                   eval      Pnsx   =  %trimr(Pnsx) + ' - '
152400080430     c                                    +  %editc(DB0nca : 'Z')
152500030515e   2c                   endif
152600030515e   1c                   endsl
152700030515      *
152800131016      * - Note (1 e 2) e Particolarità (3, 4, 5 e 6)
152900030515     c                   exsr      SchPARTIC
153000131016e   1 *
153100030716if  1c                   if           $NP(1) <> *blanks
153200030716     c                             or $NP(2) <> *blanks
153300030716     c                             or $NP(3) <> *blanks
153400030716     c                             or $NP(4) <> *blanks
153500030716     c                             or $NP(5) <> *blanks
153600030716     c                             or $NP(6) <> *blanks
153700030710     c                   eval      Pnp1   =  $NP(1)
153800030710     c                   eval      Pnp2   =  $NP(2)
153900030710     c                   eval      Pnp3   =  $NP(3)
154000030710     c                   eval      Pnp4   =  $NP(4)
154100030710     c                   eval      Pnp5   =  $NP(5)
154200030710     c                   eval      Pnp6   =  $NP(6)
154300030716e   1c                   endif
154400030515      *
154500080401      * - Privato / Appuntamento / Uffici / Consegna Garantita
154600080401      *   (servizio consegna garantita x cliente Wuerth)
154700170413if  1c*//                if        DB0gva =  'P '
154800170413     c*//                movel     'PRIVATO'     Ppad
154900170413e   1c*//                endif
155000041012if  1c                   if        DB0gva =  'U '
155100041013     c                   eval      Ppad = 'CONSEGNA AD UFFICI'
155200041012e   1c                   endif
155300030515if  1c                   if           DB0tc1 =  'A'
155400030515     c                             or DB0tc2 =  'A'
155500030515     c                   move      'APPUNTAMENTO'Ppad
155600030515e   1c                   endif
155700080401if  1c                   if             DB0nrs = 40
155800090518     c                             and  DB0tsp = 'E'
155900080401     c                             and (DB0lnp = 043
156000080401     c                              or  DB0lnp = 006
156100080401     c                              or  DB0lnp = 063)
156200090304     c                   eval      Pds2 = 'CONSEGNA ESPRESSA +
156300090304     c                                     WÜRTH'
156400080401e   1c                   endif
156500030515      *
156600030515      * - Data e ora consegna richiesta
156700030515      *   e Flag "*" > x Data e/o Ora consegna richiesta
156800091116      *   (NO "Prima del" per il cliente TTL)
156900091116if  0c                   if        DB0ccm <> c_CliTTL  or
157000091116     c                             DB0dcr =  *zeros    or  DB0tcr <> 'P'
157100030515if  1c                   if        DB0dcr <> *zeros
157200030515     c                   eval      Past   =  '*'
157300030515     c     *iso          movel     DB0dcr        Data_EUR
157400030515     c     *eur          movel     Data_EUR      Pdcr
157500030515e   1c                   endif
157600030515if  1c                   if        DB0hcr <> *zeros
157700030515     c                   eval      Past   =  '*'
157800080430     c                   eval      Phcr   =  %editw(DB0hcr : '0 :  ')
157900030515e   1c                   endif
158000091116e   0c                   endif
158100030515      *
158200030515      * - Consegne particolari
158300030515     c                   eval      Pcp1   =  DB0dc1
158400030515     c                   eval      Pcp2   =  DB0dc2
158500030515      *
158600030515      * - Imponibile / IVA / Esenzione / Totale fattura e divisa
158700030515      *   (importi "arrotondati" a 2 decimali in stampa)
158800030515if  1c                   if        DB0ift <> *zeros
158900080515     c                   eval      Pkim   = 'Imponibile'
159000080430     c                   eval(h)   w0112  =  DB0imv
159100080430     c                   eval      Pimp   =  %editc(w0112 : 'K')
159200030515sel 2c                   select
159300030515w   2c                   when      DB0iva <> *zeros
159400080515     c                   eval      Pkie   = 'I.v.a.    '
159500030624     c                   eval(h)   w0112  =  DB0iva
159600080430     c                   eval      Piva   =  %editc(w0112 : 'K')
159700030515w   2c                   when         DB0cei <> *blanks
159800030515     c                             or DB0dce <> *blanks
159900130213     c                   eval      Pkie   = 'Esenzione '
160000130213     c                   eval      Pese   = '           ' +
160100130213     c                                      %subst(DB0dce : 1 : %len(§EIdes))
160200030515e   2c                   endsl
160300080515     c                   eval      Pktf   = 'Tot. Fatt.'
160400080430     c                   eval(h)   w0112  =  DB0ift
160500080430     c                   eval      Pift   =  %editc(w0112 : 'K')
160600030515if  2c                   if        DB0div <> §GEDcn
160700030515     c                   eval      Pdiv   =  DB0div
160800030515e   2c                   endif
160900030515e   1c                   endif
161000030515      *
161100030515      * - Contrassegno e relativa divisa
161200030515if  1c                   IF        DB0cas <> *zeros
161300030625      *
161400080430     c                   eval(h)   w0112  =  DB0cas
161500080430     c                   eval      Pcas   =  %editc(w0112 : 'K')
161600030515if  2c                   if        DB0vca <> §GEDcn
161700030515     c                   eval      Pdca   =  DB0vca
161800030515e   2c                   endif
161900030515     c                   exsr      CtrCAs
162000030624      * - - Nuovo controllo abilitazione c/assegno
162100030624     c                   reset                   TRUL21DS
162200030624     c                   eval      I21cbo =  DB0cbo
162300030624     c                   eval      I21tsp =  DB0tsp
162400030624     c                   eval      I21lnp =  DB0lnp
162500030624     c                   eval      I21nzm =  DB0nzm
162600030624     c                   eval      I21lna =  DB0lna
162700030624     c                   eval      I21nzd =  DB0nzd
162800130826     c                   if        DB0ccm >  *zero
162900130826     c                   eval      I21ksc =  DB0ccm
163000130826     c                   else
163100130826     c                   eval      I21ksc =  (DB0lnp * 10000 ) + 8888
163200130826     c                   endif
163300030624     c                   eval      I21tic =  DB0tic
163400030624if  2c                   if        DB0vca <> §GEDcn
163500030624     c                   eval      I21imp =  Wcas
163600030624     c                   eval      I21div =  §GEDcn
163700030624x   2c                   else
163800030624     c                   eval      I21imp =  DB0cas
163900030624     c                   eval      I21div =  DB0vca
164000030624e   2c                   endif
164100080430     c                   eval      I21ute =  SDSusr
164200040607     c                   eval      I21pgm =  SDSpgm
164300030624     c                   call      'TRUL21R'
164400030624     c                   parm                    TRUL21ds
164500030624      * - - Carico la tabella "Tipo incasso C/Assegno" (1A)
164600030624     c                   exsr      Tab_1A
164700030624     c                   movel     §1Ades        DB0dti
164800030624if  2c                   if            §1Adeo <> *blanks
164900030624     c                             and O21fca <> *blanks
165000030624     c                   movel     §1Adeo        DB0dti
165100030624e   2c                   endif
165200030624      * - - Imposto i campi di stampa
165300080430     c                   eval      Pcti   =  DB0tic
165400080430     c                   eval      Ptic   =  DB0dti
165500030625      *
165600030515e   1c                   ENDIF
165700030515      *
165800030515      * - Totale da esigere al ritiro o alla consegna e relativa divisa
165900030515if  1c                   IF        DB0ift >  *zeros
166000030625      *
166100030515if  2c                   if            DB0vca <> §GEDcn
166200030515     c                             and DB0vca <> *blanks
166300030515     c                   eval      Pdie   =  DB0div
166400030515e   2c                   endif
166500030515      *   Controllo se C/Assegno da sommare al totale da esigere o no:
166600030515      *   se c'è il totale fattura: metto anche il il totale da esigere.
166700030515      *   (importi "arrotondati" a 2 decimali in stampa)
166800030515sel 2c                   Select
166900110204      *   Non ho C/Assegno
167000101213w   2c                   When      DB0cas  = *zeros
167100030624     c                   eval(h)   w0112  =  DB0ift
167200030624     c                   eval      Pies   =  %editc(w0112 : 'K')
167300030625     c                   clear                   Pie1
167400030625     c                   clear                   Pdi1
167500030515      *   C/Assegno sommabile
167600030515w   2c                   When      DB0ste =  'S'    and $CAeur = *on
167700030515sel 3c                   select
167800030515      *   - c/assegno in EURO
167900030515w   3c                   when      DB0div =  §GEDcn and DB0vca =  §GEDcn
168000030624     c                   eval(h)   w0112  =  DB0ift + DB0cas
168100030624     c                   eval      Pies   =  %editc(w0112 : 'K')
168200030624     c                   clear                   Pie1
168300030624     c                   clear                   Pdi1
168400030515      *   - c/assegno in DIVISA
168500030515w   3c                   when          DB0div =  §GEDcn
168600030515     c                             and DB0vca <> §GEDcn
168700030624     c                   eval(h)   w0112  =  DB0ift + Wcas
168800030624     c                   eval      Pies   =  %editc(w0112 : 'K')
168900030624     c                   clear                   Pie1
169000030624     c                   clear                   Pdi1
169100030515e   3c                   endsl
169200100218      *   C/Assegno non sommabile
169300030515x   2c                   Other
169400030624     c                   eval(h)   w0112  =  DB0cas
169500030624     c                   eval      Pie1   =  %editc(w0112 : 'K')
169600100218     c*//                eval      Pdi1   =  DB0vca
169700030624     c                   eval(h)   w0112  =  DB0ift
169800030624     c                   eval      Pies   =  %editc(w0112 : 'K')
169900100218     c*//                eval      Pdie   =  DB0div
170000030515e   2c                   Endsl
170100030709      *
170200030709e   1c                   ENDIF
170300030515      *
170400030515      * - Numero e data fattura
170500030515if  1c                   if        DB0nft <> *zeros
170600080430     c                   eval      Pkft   =  'Fatt.'
170700080430     c                   eval      Pnft   =  %editc(DB0nft : 'Z')
170800130205     c                   eval      Pfiv   = '(' + %editc(wB0fiv:'X') +
170900130205     c                                      ')'
171000030515if  2c                   if        DB0dft >  *zeros
171100080430     c                   eval      Pkdf   =  ' del '
171200030515     c     *iso          movel     DB0dft        Data_EUR
171300130213     c     *dmy          movel     Data_EUR      Pdft
171400030515e   2c                   endif
171500030515e   1c                   endif
171600030515      *
171700030515      * Memorizzo il codice tariffa come numero di copie da stampare
171800030515      *   (per Rimesso Mittente - non per DPD o FEDEX)
171900030515if  1c                   if        DB0ris = 'M'  and  *in70
172000030515     c                   eval      Wctr   =  DB0ctr
172100030515e   1c                   endif
172200030515      *
172300030515     c                   ENDSR
172400170517      *
172500170517      /free
172600170517       //--------------------------------------------------------------
172700170517       //?Controllo SE ragione sociale Mittente da FORZARE (TFNTC) per ?
172800170517       //?spedizione DPD.                                              ?
172900170517       //--------------------------------------------------------------
173000170517       BEGSR  sr_Mitt_DPD;
173100170517
173200170517         // -?Reperim. abilitazioni del Mittente dalla tab. "CLI"?
173300170517         clear  dCLI;
173400170517         reset  TIBS02ds;
173500170517         T02cod = 'CLI';
173600170517         T02ke1 = %editc( DB0ccm : 'X' );
173700170517         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
173800170517         if  T02err = *blank;
173900170517           dCLI = T02uni;
174000170517         endif;
174100170517         // -?Cliente SENZA forzatura Mittente da note "AM" e "AS"?
174200170517         if  §CLItfntc <> 'S'  and  §CLInoIMtt <> 'S';
174300170517           leavesr;
174400170517         endif;
174500170517
174600170517         // -?Forzatura Mittente da note "AM"?
174700170517         If  §CLItfntc = 'S';
174800170517
174900170517           if  NOT %open( TFNTC01L );
175000170517             open  TFNTC01L;
175100170517           endif;
175200170517           clear  keyTFNTC01;
175300170517           k_NTCapl = 'C';
175400170517           k_NTCnk1 = %editc( c_Kci : 'X' ) + %editc( DB0ccm : 'X' );
175500170517           k_NTCtnt = 'AM';
175600170517           chain  %kds( keyTFNTC01 )  TFNTC;
175700170517           if  %found( TFNTC01L )  and  NTCsns = 'S';
175800170517             DB0rsm = NTCrnt;
175900170517           endif;
176000170517
176100170517         EndIf;
176200170517
176300170517         // -?Pulizia Indirizzo Mittente?
176400170517         If  §CLInoIMtt = 'S';
176500170517
176600170517           clear  DB0inm;
176700170517           clear  DB0cam;
176800170517           clear  DB0lom;
176900170517           clear  DB0prm;
177000170517           clear  DB0nzm;
177100170517
177200170517         EndIf;
177300170517
177400170517       ENDSR;
177500170517
177600170517      /end-free
177700030515      *
177800030515      *---------------------------------------------------------------*
177900050217      *?Inschieramento delle Note e delle Particolarità              ?*
178000030515      *---------------------------------------------------------------*
178100030515     c     SchPARTIC     BEGSR
178200030515      *
178300130924     c***?(già fatto:)?  reset                   $NoDesPartic
178400130924     c***?(già fatto:)?  clear                   $NP
178500130924     c***?(già fatto:)?  clear                   W_CoOr
178600030515     c                   clear                   XX
178700030710      *
178800030710      * Imposto eventuali note per prime
178900030710if  1c                   if        DB0nt1 <> *blanks
179000030710     c                   add       1             xx
179100030710     c                   movel     DB0nt1        $NP(xx)
179200030710e   1c                   endif
179300030710if  1c                   if        DB0nt2 <> *blanks
179400030710     c                   add       1             xx
179500030710     c                   movel     DB0nt2        $NP(xx)
179600030710e   1c                   endif
179700030515      *
179800030515      * Le particolarità possono essere lunghe fino a 45 char.
179900030515      *   Se i primi 30 (della 1ª riga a disposizione) non dovessero
180000030515      *   bastare, il resto lo si mette nella riga successiva.
180100030710      *   Se, invece, non risultano decodificate, si mettono solo i
180200030710      *   codici sulla stessa riga.
180300030710      *   Se uno solo risulta decodificato, esso va su una riga a
180400030710      *   parte (vedi flag "$NoDesPartic": *off = serve riga nuova
180500030710      *                                    *on  = ok riga procedente)
180600030515      *
180700030515      * . . Particolarità Giacenza
180800030515if  1c                   IF        DB0gga <> *blanks
180900030515     c                   movel     'Q'           WTpPar
181000030515     c                   movel     DB0gga        WCdPar
181100030515     c                   exsr      TABxPAR
181200030710if  2c                   if            TBLuni       = *blanks
181300030710     c                             and $NoDesPartic = *on
181400030710     c                   eval      $NP(xx) = %trim($NP(xx))
181500030710     c                                     + '   ' + WCdPar
181600030710x   2c                   else
181700030710if  3c                   if        TBLuni       = *blanks
181800030710     c                   eval      $NoDesPartic = *on
181900030710x   3c                   else
182000030710     c                   eval      $NoDesPartic = *off
182100030710e   3c                   endif
182200030515     c                   add       1             xx
182300100517     c                   movel     w053a         $NP(xx)
182400100517if  3c                   if        %subst(w053a : %len($NP(xx))+1)
182500030515     c                              <> *blanks
182600030515     c                   add       1             xx
182700030515     c                   eval      $NP(xx) = '   ' +
182800100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
182900030710e   3c                   endif
183000030515e   2c                   endif
183100030515e   1c                   ENDIF
183200030515      *
183300030515      * . . Particolarità Consegna
183400030515if  1c                   IF        DB0gma <> *blanks
183500030515     c                   movel     'R'           WTpPar
183600030515     c                   movel     DB0gma        WCdPar
183700030515     c                   exsr      TABxPAR
183800030710if  2c                   if            TBLuni       = *blanks
183900030710     c                             and $NoDesPartic = *on
184000030710     c                   eval      $NP(xx) = %trim($NP(xx))
184100030710     c                                     + '   ' + WCdPar
184200030710x   2c                   else
184300030710if  3c                   if        TBLuni       = *blanks
184400030710     c                   eval      $NoDesPartic = *on
184500030710x   3c                   else
184600030710     c                   eval      $NoDesPartic = *off
184700030710e   3c                   endif
184800030515     c                   add       1             xx
184900100517     c                   movel     w053a         $NP(xx)
185000100517if  3c                   if        %subst(w053a : %len($NP(xx)) + 1)
185100030515     c                              <> *blanks
185200030515     c                   add       1             xx
185300030515     c                   eval      $NP(xx) = '   ' +
185400100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
185500030710e   3c                   endif
185600030515e   2c                   endif
185700030515e   1c                   ENDIF
185800030515      *
185900030515      * . . Particolarità Varie
186000030717      *     - questa descrizione va messa nell'apposito campo PPAD
186100041015      *       se è "PRIVATO" idem se è "CONSEGNA AD UFFICI"
186200030717      *     - se 'B' (bancali da rendere al mittente) comparirà un
186300030717      *       apposito box
186400030717      *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
186500030717      *       comparirà un apposito box, un'apposita nota e l'indica-
186600030717      *       zione nella copia per il destinatario.
186700030717      *     ...Potrebbero anche non esserci più elementi di schiera
186800030717      *     disponibili [xx >= %elem($NP)] !
186900030709if  1c                   IF            DB0gva <> *blanks
187000030717     c                             and DB0gva <> 'B '
187100030717     c                             and DB0gva <> 'O '
187200030709     c                             and DB0gva <> 'P '
187300041015     c                             and DB0gva <> 'U '
187400030515     c                   movel     'S'           WTpPar
187500030515     c                   movel     DB0gva        WCdPar
187600030515     c                   exsr      TABxPAR
187700030710if  2c                   if            TBLuni       = *blanks
187800030710     c                             and $NoDesPartic = *on
187900030710     c                   eval      $NP(xx) = %trim($NP(xx))
188000030710     c                                     + '   ' + WCdPar
188100030710x   2c                   else
188200030710if  3c                   if        TBLuni       = *blanks
188300030710     c                   eval      $NoDesPartic = *on
188400030710x   3c                   else
188500030710     c                   eval      $NoDesPartic = *off
188600030710e   3c                   endif
188700030717if  3c                   if            xx < %elem($NP)
188800030709     c                   add       1             xx
188900100517     c                   movel     w053a         $NP(xx)
189000100517if  4c                   if        %subst(w053a : %len($NP(xx)) + 1)
189100030709     c                              <> *blanks
189200030717     c                             and xx < %elem($NP)
189300030709     c                   add       1             xx
189400030709     c                   eval      $NP(xx) = '   ' +
189500100517     c                             %trim(%subst(w053a:%len($NP(xx))+1))
189600030717e   4c                   endif
189700030717e   3c                   endif
189800030709e   2c                   endif
189900030515e   1c                   ENDIF
190000030717      *
190100030717      * Imposto il numero originale di colli componenti la spedizione
190200030717      * e se ho ancora spazio lo memorizzo in "Note e Particolarità"
190300030717if  1c                   if        %subst(DB0gva:1:1) = 'O'
190400030717     c                   exsr      RepAR5bnb
190500030717if  2c                   if        W_CoOr  <> *blanks
190600030717     c                             and xx  <  %elem($NP)
190700030717     c                   add       1             xx
190800030717     c                   eval      $NP(xx) =  W_CoOr
190900030717e   2c                   endif
191000030717e   1c                   endif
191100030515      *
191200030515     c                   ENDSR
191300070319      *
191400070319      *---------------------------------------------------------------*
191500070319      *?Inschieramento delle Consegne Parziali precedenti            ?*
191600070319      *---------------------------------------------------------------*
191700070319     c     SchConsParz   BEGSR
191800070319      *
191900130924     c***?(già fatto:)?  reset                   $CP
192000130924     c***?(già fatto:)?  reset                   $CPdcm
192100070319     c                   clear                   XX
192200070319      *
192300070319if  2c                   if        NOT %open(FNART01L)
192400101213     c                   open(e)   FNART01L
192500101213      /free
192600101213             if  %error;
192700101213               exsr  sr_ErrOpenFile;
192800101213             endif;
192900101213      /end-free
193000070319e   2c                   endif
193100070319      *
193200070514if  2c                   if        $Mamma = *off
193300070319     c     KLDV          setll     FNART000
193400070319     c     KLDV          reade     FNART000
193500070514x   2c                   else
193600070514     c     KLBL          setll     FNART000
193700070514     c     KLBL          reade     FNART000
193800070514e   2c                   endif
193900070319      *
194000070319do  1c                   DOW       NOT  %eof(FNART01L)
194100070319      *
194200070319if  2c                   If             ARTatb = *blanks
194300070319     c                             and  ARTdcm > *zeros
194400070319      *
194500070319     c                   eval      *in31  = *off
194600070319     c                   eval      xx     = 1
194700070319     c     ARTdcm        lookup    $CPdcm(xx)                             31
194800070319if  3c                   if        NOT  *in31
194900070319     c                   eval      xx     = 1
195000070319     c     *zeros        lookup    $CPdcm(xx)                             31
195100070319if  4c                   if        *in31
195200140304     c*//                eval      $CP(xx)     = *zeros
195300140304     c                   clear                   Elem_CP
195400140304     c                   movel     Elem_CP       $CP(xx)
195500070319e   4c                   endif
195600070319e   3c                   endif
195700070319if  3c                   if        *in31
195800070319     c                   eval      $CPdcm(xx)  = ARTdcm
195900070319     c                   movel     $CP(xx)       Elem_CP
196000070319     c                   eval      WCPdcm      = ARTdcm
196100070319     c                   eval      WCPncl      = WCPncl + 1
196200070319     c                   movel     Elem_CP       $CP(xx)
196300070319e   3c                   endif
196400070319      *
196500070319e   2c                   EndIf
196600070319      *
196700070514if  2c                   if        $Mamma = *off
196800070319     c     KLDV          reade     FNART000
196900070514x   2c                   else
197000070514     c     KLBL          reade     FNART000
197100070514e   2c                   endif
197200070319      *
197300070319e   1c                   ENDDO
197400070319      *
197500140514if  0c                   If        xx     > *zero
197600140513      * Reperimento nominativo firmatario   &
197700140514     c                   exsr      sr_Firmatario
197800070319      * Ordinamento schiera per data
197900070319if  1c                   if        xx     > 1
198000070319     c                   sorta                   $CP
198100070319e   1c                   endif
198200140514e   0c                   EndIf
198300070319      *
198400070319     c                   ENDSR
198500140303      *
198600140303      /free
198700140303
198800140303       //--------------------------------------------------------------
198900140303       //?Reperimento nome firmatario                                  ?
199000140303       //--------------------------------------------------------------
199100140303       BEGSR  sr_Firmatario;
199200140303
199300140303if  1    if  NOT  %open(FNEVB22L);
199400140303           open  FNEVB22L;
199500140303e   1    endif;
199600140303
199700140303         clear  keyFNEVB22;
199800140303         k_EVB2aas = DB0aas;
199900140303         k_EVB2lnp = DB0lnp;
200000140303         k_EVB2nrs = DB0nrs;
200100140303         k_EVB2nsp = DB0nsp;
200200140303         k_EVB2cev = 'P  ';
200300140303         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
200400140303         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
200500140303         DoW  Not  %eof(FNEVB22L);
200600140303           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
200700140304             yy = %lookup( EVB2dev : $CPdcm );
200800140304             if  yy > *zero;
200900140304               Elem_CP = $CP(yy);
201000140303               if  WCPocm  <= EVB2hev;
201100140303                 WCPocm  = EVB2hev;
201200140303                 WCPfir  = EVB2dati;
201300140304                 $CP(yy) = Elem_CP;
201400140303               endif;
201500140303             endif;
201600140303           endif;
201700140304           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
201800140303         EndDo;
201900140303
202000140303       ENDSR;
202100140303
202200140303      /end-free
202300031124      *
202400031124      *---------------------------------------------------------------*
202500050217      *?Reperimento Nominativo e Telefono destinatario da FIAR5/GEN  ?*
202600031124      *---------------------------------------------------------------*
202700031124     c     RepAR5gen     BEGSR
202800031124      *
202900031124     c                   clear                   dAR5gen
203000031124      *
203100031124     c                   eval      AR5trd = 'GEN'
203200031127     c     K05AR501      chain     FIAR5000
203300031124      *
203400101213if  1c                   if            %found(FIAR501L)
203500040301     c                             and AR5atb =  *blanks
203600031124     c                   movel     AR5uni        dAR5gen
203700031124e   1c                   endif
203800031124      *
203900031124     c                   ENDSR
204000030717      *
204100030717      *---------------------------------------------------------------*
204200050217      *?Reperimento Numero Colli Originale da FIAR5/BNB              ?*
204300030717      *---------------------------------------------------------------*
204400030717     c     RepAR5bnb     BEGSR
204500030717      *
204600030717     c                   clear                   dAR5bnb
204700130924     c***?(già fatto:)?  clear                   W_CoOr
204800030717      *
204900030717     c                   eval      AR5trd = 'BNB'
205000030717     c     K05AR501      chain     FIAR5000
205100030717      *
205200101213if  1c                   if            %found(FIAR501L)
205300040301     c                             and AR5atb =  *blanks
205400030717     c                   movel     AR5uni        dAR5bnb
205500030717if  2c                   if        §AR5bcor <> *zeros
205600030717     c                   eval      W_CoOr   =  C_CoOr
205700030717     c                                      +  %editc(§AR5bcor : 'K')
205800030717e   2c                   endif
205900030717e   1c                   endif
206000030717      *
206100030717     c                   ENDSR
206200030515      *
206300030515      *---------------------------------------------------------------*
206400051129      *?Decodifiche condizionate in Lettera di Vettura               ?*
206500030515      *---------------------------------------------------------------*
206600110208     c     sr_DecodLDV   BEGSR
206700030515      *
206800030515      * Richiesta firma per ricevuta della merce senza riserve
206900030515      * (e NON  quella  per quietanza) => *in35 + not *in36
207000030515     c                   movea     '10'          *in(35)
207100070307      *
207200070307      * Intestazione moduli
207300090619      *
207400090619      /free
207500121018
207600121018         // -?Indicazione della Partiva IVA o del Cod. Fiscale del Destinatario?
207700121018         //  ?se Porto Assegnato con Fattura immediata?
207800121018         //  ?- NON se destinatario estero -?
207900121018             // -?con fattura?
208000121018         IF  (DB0nft <> *zero  or   DB0ift <> *zero)   AND     (
208100121018             // - -?P.A. (con fattura immediata)?
208200121018             //    ?(con destinatario italiano)?
208300121018             (%subst(DB0tb1:1:1) = 'A'  and  DB0nzd  = *blank)  or
208400121018             // - -?con 2ª bolla?
208500121018             //    ?(con destinatario italiano)?
208600131203              (DB0tb2 <> *blank         and  DB0nzd  = *blank) );
208700121018           select;
208800131010             // ·?Esiste Partita Iva NON FITTIZIA?
208900131010             when  wB0piv <> *blank  and
209000121026                   %scan('$$' : wB0piv) = *zero;
209100121018               PpivD = 'Partita Iva: ' + wB0piv;
209200131010             // ·?Esiste Codice Fiscale?
209300131010             when  wB0cfi <> *blank;
209400121018               PpivD = 'Codice Fiscale: ' + wB0cfi;
209500121018           endsl;
209600121018         ENDIF;
209700110204
209800110204         // -?Consegna al mattino (tempi consegna = 12 ore)?
209900110204         //  ?(NON per Würth)?
210000110204         If  §VPOrcm <= wDate and
210100090619             DB0tsp   = 'E'   and
210200090619            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
210300090619                                   DB0lnp <> 006   and
210400090619                                   DB0lnp <> 063));
210500090619           clear  TNSD99ds;
210600101213           D98tbo = 'A';
210700090619           D98aas = DB0aas;
210800090619           D98lnp = DB0lnp;
210900090619           D98nrs = DB0nrs;
211000090619           D98nsp = DB0nsp;
211100090619           tnsd99r ( TNSD99ds );
211200120910           if  D98tcc = 12;
211300090619             %subst(Pds2 : %len(Pds2) - 1) = ' M';
211400090619           endif;
211500110204         EndIf;
211600110204
211700110204         // -?"NON UTILIZZARE PER LA CONSEGNA"?
211800110204         //  ?(stampa della sola copia per il vettore)?
211900110204         if  $No_x_Cons = *on;
212000110204           *in35 = *off;
212100110204           //*in36 = *off;
212200110204           // -?costante da stampare se LDV recapito c/assegni stampata?
212300110204           //  ?dal pgm "stampa DDT NO in arrivo"?
212400110204           //Pnux   =  ' NON UTILIZZARE PER LA CONSEGNA';
212500110204         endif;
212600110204
212700110204         // -?Decodifica "Riserva Consegna" (se c'è)?
212800110204         clear  ds2A;
212900110204         If  ILVriserv <> *blank;
213000110204           TBLcod = '2A';
213100110204           TBLkey = ILVriserv;
213200110207           chain  K03TAB  TABEL;
213300110204           if  %found(TABEL00F);
213400110204             ds2A = TBLuni;
213500110204           endif;
213600110204         EndIf;
213700140514
213800140514         // -?Reperimento consegne parziali precedenti da FNART?
213900140514         If  $ConsParz;
214000140514           exsr  SchConsParz;
214100140514           if  xx < 1;
214200140514             $ConsParz = *off;
214300140514           endif;
214400140514         EndIF;
214500110204
214600110204         // -?Costante "PER RICEVUTA DELLA MERCE..."?
214700130716         exsr  sr_PerRicevutaDellaMerce;
214800110204
214900090619      /end-free
215000030515      *
215100130411      * Verifico se Packing-List e/o PDF
215200130411     c                   select
215300130411     c                   when      $§PDF  = *on
215400130411     c                   eval      Ppkl   =  'PACKING LIST PDF'
215500130411     c                   when      $FIAR8 =  *on
215600130411     c                   eval      Ppkl   =  'PACKING LIST    '
215700130411     c                   endsl
215800030515      *
215900030515      * Flag "*" > x Consegne particolari
216000030515     c                   if        DB0tc1 =  'A'  or  DB0tc2 = 'A'
216100030515     c                   eval      Past   =  '*'
216200030515     c                   endif
216300030515      *
216400030515      * C/ASSEGNO - - - - - - - - - - - - - - - - - - - - - - - - - - *
216500030515if  1c                   IF        DB0cas <> *zeros
216600030515      * - Flag "*" > x C/Assegno
216700030904if  2c                   if        O21fca <> *blanks
216800030904     c                   movel     '*'           Past
216900030515e   2c                   endif
217000030624      * - Imposto i campi di stampa
217100080430     c                   eval      Pcti   =  DB0tic
217200080430     c                   eval      Ptic   =  DB0dti
217300030515e   1c                   ENDIF
217400030515      *
217500040607      * TIPO LDV  - - - - - - - - - - - - - - - - - - - - - - - - - - *
217600030515      * - Se Bolla in Assegnato: stampo una "A" vicino all'asterisco
217700030515      * e se Bolla in Assegnato Senza Fattura: stampo "SEGUE FATTURA"
217800030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
217900030515     c                             or        DB0tb2     <> *blanks
218000030515     c                   eval      Pass   =  'A'
218100030515if  2c                   if        DB0nft =  *zeros
218200080430     c                   if        Pkie   =  *blanks
218300080430     c                   evalr     Pkie   =  'SEGUE'
218400080430     c                   eval      Piva   =  ' FATTURA'
218500080430     c                   else
218600080430     c                   eval      Piva   =  'SEGUE FATTURA'
218700080430     c                   endif
218800030515e   2c                   endif
218900030515e   1c                   endif
219000030515      *
219100030515      * DECODIFICA TIPO CONSEGNA RICHIESTA  - - - - - - - - - - - - - *
219200051129if  1c                   IF            DB0dcr >  *zeros
219300051129     c                             or  DB0hcr >  *zeros
219400051129     c                   clear                   FNLV80ds
219500051129     c                   eval      ILV80aas   =  DB0aas
219600051129     c                   eval      ILV80lnp   =  DB0lnp
219700051129     c                   eval      ILV80nrs   =  DB0nrs
219800051129     c                   eval      ILV80nsp   =  DB0nsp
219900051129     c                   eval      ILV80mgs   =  DB0mgs
220000051129     c                   eval      ILV80tc1   =  DB0tc1
220100051129     c                   eval      ILV80tc2   =  DB0tc2
220200051129     c                   eval      ILV80dcr   =  DB0dcr
220300051129     c                   eval      ILV80hcr   =  DB0hcr
220400051129     c                   eval      ILV80tcr   =  DB0tcr
220500051129     c                   call      'FNLV80R'
220600051129     c                   parm                    KPJBA
220700051129     c                   parm                    FNLV80ds
220800051129sel 2c                   Select
220900051129      * errore
221000051129w   2c                   When      OLV80err  <>  *blanks
221100051129      * consegna tassativa (per ora solo ai supermercati)
221200051129w   2c                   When      OLV80tat   =  'S'
221300081021     c                   movel     OLV80tcrd     Ptdr
221400091116      * NO "Prima del" per il cliente TTL
221500091116w   2c                   When      DB0ccm     = c_CliTTL   and
221600091116     c                             DB0dcr    <> *zeros     and  DB0tcr = 'P'
221700051129      * prima/dopo il/delle
221800051129      * (il giorno e/o l'ora di consegna richiesta sono già impostati
221900110208      *  nella subr. "sr_TipoLDV")
222000081022x   2c                   Other
222100081021     c                   movel     OLV80tcrd     Ptdr
222200081021e   2c                   Endsl
222300051129e   1c                   ENDIF
222400030515      *
222500030515      * DECODIFICA DEI TURNI DI CHIUSURA  - - - - - - - - - - - - - - *
222600060921      * (SE?non?è "Fermo Deposito")
222700030515      * 1° turno di chiusura
222800060921if  1c                   IF             DB0gc1 <> *blanks
222900060921     c                             and  DB0ffd  = *blanks
223000030515      * - Flag "*" > Turno di chiusura
223100030515     c                   eval      Past   =  '*'
223200030515if  2c                   If        %subst(DB0gc1:1:1) = *blank
223300030515      * - Nessun giorno specificato - Solo 'M' o 'P'
223400130924     c***?già così:?     clear                   Pgc1
223500030515if  3c                   if        DB0gc1 =  ' M'
223600030515     c                   eval      Pgc1   =  %trim($Gio(8))
223700030515x   3c                   else
223800030515     c                   eval      Pgc1   =  %trim($Gio(9))
223900030515e   3c                   endif
224000030515x   2c                   Else
224100030515      * - Giorno specificato
224200030515     c                   movel     DB0gc1        w001a
224300030515     c                   eval      XX     =  1
224400030515     c                   eval      *in31  =  *off
224500030515     c     w001a         lookup    $GgS(xx)                               31
224600030515     c   31              movel     $Gio(xx)      Pgc1
224700030515      *   + 'M' o 'P'
224800030515sel 3c                   select
224900030515w   3c                   when      %subst(DB0gc1:2:1) = 'M'
225000030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
225100030515     c                                    +  %trim($Gio(8))
225200030515w   3c                   when      %subst(DB0gc1:2:1) = 'P'
225300030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
225400030515     c                                    +  %trim($Gio(9))
225500030515e   3c                   endsl
225600030515e   2c                   Endif
225700030515e   1c                   ENDIF
225800030515      * 2° turno di chiusura
225900060921if  1c                   IF             DB0gc2 <> *blanks
226000060921     c                             and  DB0ffd  = *blanks
226100030515      * - Flag "*" > Turno di chiusura
226200030515     c                   eval      Past   =  '*'
226300030515if  2c                   If        %subst(DB0gc2:1:1) = *blank
226400030515      * - Nessun giorno specificato - Solo 'M' o 'P'
226500130924     c***?già così:?     clear                   Pgc2
226600030515if  3c                   if        DB0gc2 =  ' M'
226700080515     c                   eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
226800080515     c                                                = %trim($Gio(8))
226900030515x   3c                   else
227000080515     c                   eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
227100080515     c                                                = %trim($Gio(9))
227200030515e   3c                   endif
227300030515x   2c                   Else
227400030515      * - Giorno specificato
227500030515     c                   movel     DB0gc2        w001a
227600030515     c                   eval      XX     =  1
227700030515     c                   eval      *in31  =  *off
227800030515     c     w001a         lookup    $GgS(xx)                               31
227900080515     c   31              eval      %subst(Pgc2:%len(Pgc2)-%len(Pgc1)+1)
228000080515     c                                                = %trim($Gio(xx))
228100030515      *   + 'M' o 'P'
228200030515sel 3c                   select
228300030515w   3c                   when      %subst(DB0gc2:2:1) = 'M'
228400080513     c                   eval      Pgc2   =  %trimr(Pgc2) + ' '
228500030515     c                                    +  %trim($Gio(8))
228600030515w   3c                   when      %subst(DB0gc2:2:1) = 'P'
228700080513     c                   eval      Pgc2   =  %trimr(Pgc2) + ' '
228800030515     c                                    +  %trim($Gio(9))
228900030515e   3c                   endsl
229000030515e   2c                   Endif
229100030515e   1c                   ENDIF
229200080523      /free
229300080523       // ________________________________________
229400080523       // Impostazione nominativo del destinatario
229500080523       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
229600110214         exsr  RepAR4_1;
229700080523       // _______________________________________
229800080523       // Impostazione data ed ora della consegna
229900080523       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
230000110228         //?ATTENZIONE:?il "RESET" di un campo di tipo "d" (data)?
230100110228         //?riporta in tale campo la *date, NON quella impostata?
230200110228         //?come "inz(...)" della specifica "d" !!!!!!!!!!!!!!!!?
230300110228         Data_EUR = *loval;
230400110209         clear  w0080;
230500110209         select;
230600110209           when  wB0dcm > *zero;
230700110209             Data_EUR = %date( wB0dcm : *iso );
230800110209           when  wB0dcp > *zero;
230900110209             Data_EUR = %date( wB0dcp : *iso );
231000110209         endsl;
231100110209         if  Data_EUR > *loval;
231200110209           w0080 = ( ( %subdt(Data_EUR : *days) * 1000000 ) +
231300110209                     ( %subdt(Data_EUR : *months) * 10000 ) +
231400110209                     ( %subdt(Data_EUR : *years) ) );
231500110209         endif;
231600150430         if  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
231700081103           Pfedkcd  = %editc( w0080 : 'Y' );
231800120702           if  dILVlibero.§ILVhmp > *zero;
231900081103             Pfedkcd = %trimr( Pfedkcd ) + '  '
232000120702                     + %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':'
232100120702                     + %subst( dILVlibero.§ILVhmp : 3 : 2 );
232200081103           endif;
232300081103         else;
232400081103           Pkcd  = %editc( w0080 : 'Y' );
232500120702           if  dILVlibero.§ILVhmp > *zero;
232600081103             Pkcd = %trimr( Pkcd ) + '   '
232700120702                  + %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':'
232800120702                  + %subst( dILVlibero.§ILVhmp : 3 : 2 );
232900081103           endif;
233000081103         endif;
233100080523      /end-free
233200071114      *
233300071114      * ALTRI DATI/FLAG/... - - - - - - - - - - - - - - - - - - - - - *
233400030515      *
233500071114      * Se pgm richiamato x stampa LdV in arrivo:
233600030515if  1c                   IF        DB0fl1 =  'A'
233700071114      * - imposto l'"*" in base al parametro ricevuto.
233800030515if  2c                   if        DB0fl2 =  '*'
233900030515     c                   eval      Past   =  '*'
234000030515x   2c                   else
234100030515     c                   clear                   Past
234200030515e   2c                   endif
234300071114      * - idem per flag di bolla non abilitata
234400030515     c                   eval      Pdis   =  DB0fl5
234500030515e   1c                   endif
234600030515      *
234700030515     c                   ENDSR
234800080430      *
234900080430      /free
235000130716
235100130716       //--------------------------------------------------------------
235200130716       //?Impostazione "PER RICEVUTA DELLA MERCE ..."                  ?
235300130716       //--------------------------------------------------------------
235400130716       BEGSR  sr_PerRicevutaDellaMerce;
235500130716
235600130716         Select;
235700130716
235800130716           // -?danon?stampare se?
235900130716           // ·?non utilizzare per la consegna  (NOT *in35)?
236000130716           When  NOT *in35;
236100130716             //clear  Pkrm;       ?(già fatto!)?
236200140108             //clear  Pkrm2;      ?(già fatto!)?
236300130716
236400130716           // -?costante per "DPD"   (come standard)?
236500130716           //         ?o per "FedEx" (come standard)?
236600130716           When  *in71  or  *in72;
236700130716             Pkrm = 'PER RICEVUTA DELLA MERCE';
236800130716             // -?senza riserve?
236900130716             if  ILVriserv = *blank;
237000130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
237100130716             // -?con Riserva Consegna?
237200130716             else;
237300130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
237400130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
237500130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
237600130716               endif;
237700130716             endif;
237800130716
237900130716           // -?costante per cliente?
238000130716           //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
238100140513           When  DB0ccm = c_CliCLN  or  DB0ccm = c_CliLN_2;
238200130716             exsr  sr_RepDdtCLN;
238300130716             Pkrm = 'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
238400130716                     DDT '
238500130716                  + %trimr( %editc (WddtCLN : 'Z') )
238600130716                  + ' DEL '
238700130716                  + %editc (WdspCLN : 'Y')
238800130716                  + ' CON RISERVA DI CONTROLLO COME CONTRATTO';
238900130716             if  Wdsp < 20101001;
239000130716               Pkrm = %trimr(Pkrm) + ' C.L.N.';
239100130716             else;
239200130716               Pkrm = %trimr(Pkrm) + ' L.N. S.R.L.';
239300130716             endif;
239400130716
239500130716           // -?costante "standard" se "DDT sì"?
239600130716           When  §VPOrsr <= wDate  and  DB0bam = 'S';
239700130716             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
239800130716                     TRASPORTO';
239900130716             // -?senza riserve?
240000130716             if  ILVriserv = *blank;
240100130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
240200130716             // -?con Riserva Consegna?
240300130716             else;
240400130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
240500130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
240600130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
240700130716               endif;
240800130716             endif;
240900130716
241000130716           // -?costante "standard" se "DDT no" + Packing-List?
241100161012           When  DB0bam = *blank  and  ($FIAR8 = *on  or  $§PDF = *on);
241200130716             Pkrm = 'PER RICEVUTA DELLA MERCE E DEL DOCUMENTO DI +
241300130716                     TRASPORTO';
241400130716             // -?senza riserve?
241500130716             if  ILVriserv = *blank;
241600130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
241700130716             // -?con Riserva Consegna?
241800130716             else;
241900130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
242000130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
242100130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
242200130716               endif;
242300130716             endif;
242400130716
242500130716           // -?costante "standard" altrimenti?
242600130716           Other;
242700130716             Pkrm = 'PER RICEVUTA DELLA MERCE';
242800130716             // -?senza riserve?
242900130716             if  ILVriserv = *blank;
243000130716               Pkrm = %trimr(Pkrm) + ' SENZA RISERVE';
243100130716             // -?con Riserva Consegna?
243200130716             else;
243300130716               Pkrm = %trimr(Pkrm) + ' CON ' + §2Ades;
243400130716               if  ILVriserv = c_RiservaSpecif  and  ILVnotAut <> *blank;
243500130716                 Pkrm = %trimr(Pkrm) + ': ' + ILVnotAut;
243600130716               endif;
243700130716             endif;
243800130716
243900130716         EndSl;
244000130716
244100130716         // -?costante aggiuntiva per cliente?
244200130716         //  ?"S G Soluzioni Logistiche"?
244300130716         If  DB0ccm = c_CliSG   or   DB0ccm = c_CliSG_2;
244400130716
244500130716           if  DB0nas <> *blank   and   DB0nas <> c_NoContr;
244600130716             Pkrm2 = 'INCLUDE CONTRATTO - '
244700130716                   + %trim(DB0nas);
244800130716           endif;
244900130716           if  DB0rmo <> *blank;
245000130716             if  Pkrm2 <> *blank;
245100130716               Pkrm2 = %trimr(Pkrm2)
245200130716                     + ' - MATR TV: '
245300130716                     + %trim(DB0rmo);
245400130716             else;
245500130716               Pkrm2 = 'MATR TV: '
245600130716                     + %trim(DB0rmo);
245700130716             endif;
245800130716           endif;
245900130716
246000130716         EndIf;
246100140108
246200140108         // -?Consegne Parziali Precedenti?
246300140108         If  $ConsParz = *on  and  wB0dcm = *zero;
246400140108
246500140108           Select;
246600140110             When  Pkrm2 = *blank;
246700140110               Pkrm2 = %subst( Pkrm2 : 1 : 110 ) + ' PARZIALE';
246800140108             When  %subst( Pkrm : 111 ) = *blank;
246900140108               Pkrm = %subst( Pkrm : 1 : 110 ) + ' PARZIALE';
247000140108             When  %subst( Pkrm2 : 111 ) = *blank;
247100140108               Pkrm2 = %subst( Pkrm2 : 1 : 110 ) + ' PARZIALE';
247200140108           EndSl;
247300140108
247400140108         EndIf;
247500130716
247600130716       ENDSR;
247700090619
247800090619       //--------------------------------------------------------------
247900130716       //?Reperimento Numero e Data del DDT originale del mittente     ?
248000130716       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
248100090619       //--------------------------------------------------------------
248200090619       BEGSR  sr_RepDdtCLN;
248300090619
248400130716         // -?Verifica se la spedizione ha mamma?
248500090619         if  not %open(FNLBL01L);
248600101213           open(e)  FNLBL01L;
248700101213           if  %error;
248800101213             exsr  sr_ErrOpenFile;
248900101213           endif;
249000090619         endif;
249100090619         chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
249200090619
249300090619         Select;
249400090619
249500130716           // -?Con Mamma: reperimento dati DDT dalla mamma?
249600090619           When  %found(FNLBL01L);
249700090619             if  not %open(FNBLP01L);
249800101213               open(e)  FNBLP01L;
249900101213               if  %error;
250000101213                 exsr  sr_ErrOpenFile;
250100101213               endif;
250200090619             endif;
250300090619             chain  (LBLaao : LBLlpo : LBLnro : LBLnso)  FNBLP000;
250400090619             if  %found(FNBLP01L);
250500090619               exsr  sr_DatiCLN_blp;
250600090619             endif;
250700090619
250800130716           // -?In Arrivo: reperimento dati DDT da FNARB?
250900090619           Other;
251000090619             exsr  sr_DatiCLN_arb;
251100090619
251200090619         EndSl;
251300090619
251400090619       ENDSR;
251500090619
251600090619       //--------------------------------------------------------------
251700090619       //?In Partenza: reperimento dati DDT da FNBLP                   ?
251800090619       //--------------------------------------------------------------
251900090619       BEGSR  sr_DatiCLN_blp;
252000090619
252100130716         // -?Numero DDT?
252200090619         select;
252300090619           when  BLPrmn <> *zero;
252400100906             if  BLPrmn  > -99999999   and
252500100906                 BLPrmn <   99999999;
252600100906               WddtCLN = BLPrmn;
252700100906             endif;
252800090619           when  BLPncd <> *zero;
252900090619             WddtCLN = BLPncd;
253000090619         endsl;
253100090619
253200130716         // -?Data DDT in formato gg/mm/aaaa?
253300090619         Data_Dmy = %date( ((BLPaas * 10000) + BLPmgs) : *iso );
253400100906         WdspCLN  = ( ( %subdt(Data_Dmy : *days)  * 10000 ) +
253500100906                      ( %subdt(Data_Dmy : *months) * 100 ) +
253600100906                ( %int( %subst( %editc(BLPaas : 'X') : 3 : 2 ) ) ) );
253700090619
253800090619       ENDSR;
253900090619
254000090619       //--------------------------------------------------------------
254100090619       //?In Arrivo: reperimento dati DDT da FNARB                     ?
254200090619       //--------------------------------------------------------------
254300090619       BEGSR  sr_DatiCLN_arb;
254400090619
254500130716         // -?Numero DDT?
254600090619         select;
254700090619           when  ARBrmn <> *zero;
254800100906             if  ARBrmn  > -99999999   and
254900100906                 ARBrmn <   99999999;
255000100906               WddtCLN = ARBrmn;
255100100906             endif;
255200090619           when  ARBncd <> *zero;
255300090619             WddtCLN = ARBncd;
255400090619         endsl;
255500090619
255600130716         // -?Data DDT in formato gg/mm/aaaa?
255700090619         Data_Dmy = %date( ((ARBaas * 10000) + ARBmgs) : *iso );
255800090619         WdspCLN  = ( ( %subdt(Data_Dmy : *days)  * 10000 ) +
255900090619                      ( %subdt(Data_Dmy : *months) * 100 ) +
256000110117                ( %int( %subst( %editc(ARBaas : 'X') : 3 : 2 ) ) ) );
256100090619
256200090619       ENDSR;
256300100517
256400100517       //--------------------------------------------------------------
256500100517       //?Verifica se presente ORM con preavviso mail                  ?
256600100517       //--------------------------------------------------------------
256700110117       BEGSR  sr_CheckORM_PreavvMail;
256800100517
256900130924         //clear  dOrm01;             ?(già fatto)?
257000100517
257100130925         // -?Reperimento num. ORM?
257200130924         exsr  RepAR4_M;
257300100517
257400130925         // -?SE ORM inesistente: inutile proseguire?
257500130925         if  §B4poe = *zero   or   §B4nor = *zero;
257600100517           leavesr;
257700100517         endif;
257800100517
257900130716         // -?Verifica presenza O.R.M.?
258000100517         if  Not  %open(FNORM01L);
258100101213           open(e)  FNORM01L;
258200101213           if  %error;
258300101213             exsr  sr_ErrOpenFile;
258400101213           endif;
258500100517         endif;
258600100517
258700100517         ORMpoe = §B4poe;
258800100517         ORMnsr = §B4nsr;
258900100517         ORMnor = §B4nor;
259000100517         ORMnrv = §B4nrv;
259100100517
259200100517         chain  K04ORM01  FNORM000;
259300100517
259400130716         // -?ORM non reperito: inutile proseguire?
259500100517         if  Not  %found(FNORM01L);
259600100517           leavesr;
259700100517         endif;
259800100517
259900130716         // -?Impostazione flag per stampa porto "ASSEGNATO"?
260000100517         dOrm01 = ORMflo;
260100100517         $PreavvORM = (§ORMpre = 'M');
260200100517
260300100517       ENDSR;
260400090619
260500080430       //--------------------------------------------------------------
260600140303       //?Preparazione dati di output relativi alla Lettera di Vettura ?
260700080430       //--------------------------------------------------------------
260800131016       BEGSR  sr_DatiOUT;
260900080430
261000101221         // -?Impostazione righe?
261100080430         clear LdV;
261200080430         clear wx;
261300080430
261400101221         // -?Area immagine LdV?
261500080801
261600110203         FOR  wx = 1  TO  c_Row;
261700080430
261800080430           %occur(LdV) = wx;
261900080430
262000080508           LdvPrg      = wx;
262100080430           select;
262200080430
262300101221             // ·? 1ª riga?
262400110203             when  wx = 1;
262500110203             LdvTxt = ds_01;
262600080430
262700101221             // ·? 2ª riga (vuota)?
262800110203             when  wx = 2;
262900080430             clear LdvTxt;
263000080430
263100101221             // ·? 3ª riga?
263200110203             when  wx = 3;
263300110203             LdvTxt = ds_03;
263400080430
263500101221             // ·? 4ª riga?
263600110203             when  wx = 4;
263700110203             LdvTxt = ds_04;
263800080430
263900101221             // ·? 5ª riga?
264000110203             when  wx = 5;
264100110203             LdvTxt = ds_05;
264200080430
264300101221             // ·? 6ª riga (vuota)?
264400110203             when  wx = 6;
264500110203             clear LdvTxt;
264600080430
264700101221             // ·? 7ª riga?
264800110203             when  wx = 7;
264900110203             LdvTxt = ds_07;
265000080430
265100101221             // ·? 8ª riga (vuota)?
265200110203             when  wx = 8;
265300080430             clear LdvTxt;
265400080430
265500101221             // ·? 9ª riga?
265600110203             when  wx = 9;
265700110203             LdvTxt = ds_09;
265800080430
265900101221             // ·?10ª riga?
266000110203             when  wx = 10;
266100110203             LdvTxt = ds_10;
266200080430
266300101221             // ·?11ª riga?
266400110203             when  wx = 11;
266500110203             LdvTxt = ds_11;
266600080430
266700101221             // ·?12ª riga?
266800110203             when  wx = 12;
266900110203             LdvTxt = ds_12;
267000080430
267100101221             // ·?13ª riga?
267200110203             when  wx = 13;
267300110203             LdvTxt = ds_13;
267400080430
267500101221             // ·?14ª riga?
267600110203             when  wx = 14;
267700110203             LdvTxt = ds_14;
267800080430
267900101221             // ·?15ª riga?
268000110203             when  wx = 15;
268100110203             LdvTxt = ds_15;
268200080430
268300101221             // ·?16ª riga?
268400110203             when  wx = 16;
268500110203             LdvTxt = ds_16;
268600080430
268700101221             // ·?17ª riga?
268800110203             when  wx = 17;
268900110203             LdvTxt = ds_17;
269000080430
269100101221             // ·?18ª riga?
269200110203             when  wx = 18;
269300110203             LdvTxt = ds_18;
269400080430
269500101221             // ·?19ª riga?
269600110203             when  wx = 19;
269700110203             LdvTxt = ds_19;
269800080430
269900101221             // ·?20ª riga?
270000110203             when  wx = 20;
270100110203             LdvTxt = ds_20;
270200080430
270300101221             // ·?21ª riga?
270400110203             when  wx = 21;
270500110203             LdvTxt = ds_21;
270600080430
270700101221             // ·?22ª riga?
270800110203             when  wx = 22;
270900110203             LdvTxt = ds_22;
271000080430
271100101221             // ·?23ª riga?
271200110203             when  wx = 23;
271300110203             LdvTxt = ds_23;
271400080430
271500101221             // ·?24ª riga?
271600110203             when  wx = 24;
271700110203             LdvTxt = ds_24;
271800080430
271900101221             // ·?25ª riga?
272000110203             when  wx = 25;
272100110203             LdvTxt = ds_25;
272200080430
272300101221             // ·?26ª riga (vuota)?
272400110203             when  wx = 26;
272500080806             clear LdvTxt;
272600080430
272700101221             // ·?27ª riga?
272800110203             when  wx = 27;
272900110203             LdvTxt = ds_27;
273000080430
273100101221             // ·?28ª riga?
273200110203             when  wx = 28;
273300110203             LdvTxt = ds_28;
273400081021
273500101221             // ·?29ª riga?
273600110203             when  wx = 29;
273700081021             LdvTxt = ds_29;
273800081021
273900101221             // ·?30ª riga?
274000110203             when  wx = 30;
274100081021             LdvTxt = ds_30;
274200081021
274300110215             // ·?31ª riga?
274400110203             when  wx = 31;
274500110215             LdvTxt = ds_31;
274600081021
274700101221             // ·?32ª riga?
274800110203             when  wx = 32;
274900081021             LdvTxt = ds_32;
275000081021
275100101221             // ·?33ª riga?
275200110203             when  wx = 33;
275300081021             LdvTxt = ds_33;
275400081021
275500101221             // ·?34ª riga?
275600110203             when  wx = 34;
275700081021             LdvTxt = ds_34;
275800081021
275900101221             // ·?35ª riga (vuota)?
276000110203             when  wx = 35;
276100081021             clear LdvTxt;
276200081021
276300101221             // ·?36ª riga (vuota)?
276400110203             when  wx = 36;
276500081021             clear LdvTxt;
276600080430
276700080430           endsl;
276800080430
276900080430         ENDFOR;
277000080430
277100110518         // -?Impostazione iniziale del modulo standard:?
277200131203         //// ·?Modulo standard con logo Bartolini?
277300131203         //TemplateName = 'ldvbase00 ';
277400131203         //// ·?Modulo standard con logo Bartolini-BRT?
277500131203         //TemplateName = 'ldvbase10 ';
277600120524         // ·?Modulo standard con logo BRT?
277700120524         TemplateName = 'ldvbase20 ';
277800080801
277900110203         Select;
278000081030
278100101221           // -?Area dati "DPD"?
278200101221           //  ?(NON della LNP, ma dalla fil. ricavata dal cod.mittente)?
278300110203           When  §CMntw = 'DPD';
278400110203             exsr  sr_LdVbaseDPD;
278500081030
278600101221           // -?Area dati "FedEx"?
278700150430           When  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
278800110203             exsr  sr_LdVbaseFED;
278900081030
279000131206           // -?Area box (ex "Promemoria per l'autotrasportatore")?
279100131206           When  $1o_Passaggio = *on  or  $ConsParz = *on;
279200110203             exsr  sr_LdVbase01;
279300081030
279400110203         EndSl;
279500130411
279600130411
279700130411         // ? CHIODI PER TEST                               ?
279800130411         Select;
279900130411           // -?Nessun TEST?
280000130411           When  $Test = *blank;
280100130411           // -?Spedizione FedEx?
280200130411           When  $Test = 'FED';
280300130411             TemplateName = 'ldvbase23 ';
280400130411           // -?Spedizione DPD?
280500130411           When  $Test = 'DPD';
280600130411             TemplateName = 'ldvbase22 ';
280700130411           // -?Spedizione con Consegna Parziale Precedente?
280800130411           When  $Test = 'C_P';
280900130411             TemplateName = 'ldvbase21 ';
281000130411           // -?Spedizione Normale?
281100130411           When  $Test = 'NOR';
281200130411             TemplateName = 'ldvbase20 ';
281300130411         EndSl;
281400130411
281500080801
281600101221         // -?Vuoti finali?
281700130213         For  wx = wx  To  w_Row;
281800080801           %occur(LdV) = wx;
281900130213           if  LdVPrg = *zero;
282000130213             LdvPrg = wx;
282100130213             clear  LdvTxt;
282200130213           endif;
282300110203         EndFor;
282400130205
282500130205         // -?Dati aggiuntivi per la 14ª riga (esenzione Iva)?
282600130205         if  DB0ift <> *zero  and
282700130205             DB0iva =  *zero  and
282800130205            (DB0cei <> *blank  or  DB0dce <> *blank);
282900130213           w_Row += 1;
283000130213           wx = w_Row;
283100130205           %occur(LdV) = wx;
283200130205           LdvPrg = 140;
283300130205           LdvTxt = ds_140;
283400130205         endif;
283500130205         // -?Dati aggiuntivi per la 18ª riga (n° fatt. e fil. Iva)?
283600130205         if  DB0nft <> *zero;
283700130213           w_Row += 1;
283800130213           wx = w_Row;
283900130205           %occur(LdV) = wx;
284000130205           LdvPrg = 180;
284100130205           LdvTxt = ds_180;
284200130205         endif;
284300110201
284400110201         // -?Codice a barre: ANNO+LNP+NRS+NSP+CKD:?
284500110201         //  ?richiama il pgm TRUL28R per ricavarne il check digit?
284600110201         reset  TRUL28ds;
284700110201         I28cod = BarCodeDS;
284800110201         TRUL28R1 ( TRUL28ds );
284900110201
285000110201         // -?Impostazione campo "LastRow"?
285100110201         LastRow = 'Spedizione: ' + %subst( O28cod : 1 : 15 ) + '  ' +
285200110203                   'Distinta: ' + %subst( ILVprmDst : 1 : 3 ) + '-'
285300170707                                + %subst( ILVprmDst : 4 ) +
285400170707                   '  (' + dILVlibero.§ILVapl + ')';
285500080801
285600110203         // ______________________________
285700110203         //?Stampa di prova (SE richiesta)?
285800110203         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
285900110519         If  SDSprm > 11;
286000130716           exsr  sr_Print;
286100130716         EndIf;
286200080430
286300080430       ENDSR;
286400130716
286500130716       //--------------------------------------------------------------
286600130716       //?Stampa dei dati per l'immagine - SE RICHIAMATO PER PROVA     ?
286700130716       //--------------------------------------------------------------
286800130716       BEGSR  sr_Print;
286900130716
287000130716         // -?Apertura file di stampa?
287100130716         if not %open(QSYSPRT);
287200130716           open  QSYSPRT;
287300130716         endif;
287400130716
287500130716         // -?Stampa "testata"?
287600130716         except PRTtxt;
287700130716
287800130716         // -?Stampa dettaglio?
287900130716         for wx = 1 to w_Row;
288000130716           %occur(LdV) = wx;
288100130716           except PRTrow;
288200130716         endfor;
288300130716
288400130716         // -?Stampa "ultima riga"?
288500130716         except PRTend;
288600130716
288700130716         // -?Chiusura file di stampa?
288800130716         close QSYSPRT;
288900130716
289000130716       ENDSR;
289100081030
289200081030       //--------------------------------------------------------------
289300130716       //?Preparazione dati x spedizione "DPD"                         ?
289400081030       //--------------------------------------------------------------
289500130716       BEGSR  sr_LdVbaseDPD;
289600081030
289700120524         // -?Impostazione del modulo DPD:?
289800131203         //// ·?Modulo DPD con logo Bartolini?
289900131203         //TemplateName = 'ldvbase02 ';
290000131203         //// ·?Modulo DPD con logo Bartolini/BRT?
290100131203         //TemplateName = 'ldvbase12 ';
290200120524         // ·?Modulo DPD con logo BRT?
290300120524         TemplateName = 'ldvbase22 ';
290400081030
290500101221         // -?Impostazione data?
290600081030         %occur(LdV) = 19;
290700081030         %subst(LdVTxt : 94) = 'Date ' + PdspA;
290800081030
290900101221         // -?Impostazione numero colli?
291000081030         %occur(LdV) = 20;
291100081030         %subst(LdVTxt : 94) = 'Parcel N.' + PnclA;
291200081030
291300101221         // -?Impostazione numero parcel?
291400081030         %occur(LdV) = 21;
291500081030         %subst(LdVTxt : 94) = §B4Ipn;
291600081111
291700131015         // -?"Decodifica" del barcode?
291800081111         %occur(LdV) = 22;
291900081111         %subst(LdVTxt : 94) = *blank;
292000081111         if  §B4Ipn <> *blank;
292100101221           // -?controllo se vecchio o nuovo barcode?
292200081111           if  %subst(§B4Ipn : 14 : 1) = *blank;
292300081111             // - - vecchio
292400081111             %subst(LdVTxt : 94) = '01 ' + %subst(§B4Ipn : 1 : 3)
292500081111                                 + ' '   + %subst(§B4Ipn : 4 : 8);
292600081111           else;
292700081111             // - - nuovo
292800081111             %subst(LdVTxT : 94) = '01 ' + %subst(§B4Ipn : 1 :  4)
292900081111                                 + ' '   + %subst(§B4Ipn : 5 : 10);
293000081111           endif;
293100081111         endif;
293200081111
293300130716         // -?Impostazione costanti  (LB ha detto di NON metterle)?
293400081111         //%occur(LdV) = 23;
293500081111         //%subst(LdVTxt : 94) = *blank;
293600081111         //%occur(LdV) = 24;
293700081111         //%subst(LdVTxt : 94) = 'Controllare la corrispon-';
293800081111         //%occur(LdV) = 25;
293900081111         //%subst(LdVTxt : 94) = 'denza del destinatario con';
294000081111         //%occur(LdV) = 26;
294100081111         //%subst(LdVTxt : 94) = 'l''indirizzo dell''etichetta';
294200081111         //%occur(LdV) = 27;
294300081111         //%subst(LdVTxt : 94) = 'DPD.';
294400081030
294500081030       ENDSR;
294600081030
294700081030       //--------------------------------------------------------------
294800130716       //?Preparazione dati x spedizione "FedEx"                       ?
294900081030       //--------------------------------------------------------------
295000130716       BEGSR  sr_LdVbaseFED;
295100081030
295200120524         // -?Impostazione del modulo FedEx:?
295300131203         //// ·?Modulo FedEx con logo Bartolini?
295400131203         //TemplateName = 'ldvbase03 ';
295500131203         //// ·?Modulo FedEx con logo Bartolini/BRT?
295600131203         //TemplateName = 'ldvbase13 ';
295700120524         // ·?Modulo FedEx con logo BRT?
295800120524         TemplateName = 'ldvbase23 ';
295900110517
296000081103         w_Row  = c_Row_B;
296100081030         Wtrc = 'C';
296200081030
296300101221         // -?Preparazione elenco segnacolli FedEx?
296400101213         if  NOT %open(FNART01L);
296500101213           open(e)  FNART01L;
296600101213           if  %error;
296700101213             exsr  sr_ErrOpenFile;
296800101213           endif;
296900101213         endif;
297000101213         if  NOT %open(FIARS01L);
297100101213           open(e)  FIARS01L;
297200101213           if  %error;
297300101213             exsr  sr_ErrOpenFile;
297400101213           endif;
297500101213         endif;
297600081030
297700101221         // -?Impostazione numeratore fino alla 39ª riga?
297800101213         FOR  wx = wx  TO (w_Row - c_Row_Fed);
297900081030
298000081030           %occur(LdV) = wx;
298100081030           LdvPrg      = wx;
298200081030
298300101213         ENDFOR;
298400081030
298500101221         // -?40ª riga & successive?
298600081030         clear  xx;
298700081030         clear  Wnsc;
298800101221         FOR  wx = (w_Row - c_Row_Fed + 1)  TO w_Row;
298900081030
299000081030           %occur(LdV) = wx;
299100081030           LdvPrg      = wx;
299200081030
299300081030           // - Caricamento AWB FedEx
299400081103           clear  wAWB;
299500101221           // - -?leggo segnacolli per bolle in arrivo?
299600081030           exsr  CarAWB_Arx;
299700081030           if  wAWB = *blank;
299800081030             leave;
299900081030           endif;
300000081030
300100081030           xx    += 1;
300200081103           Pptn   = wAWB;
300300081030           Psts   = %editc( xx : 'Z' );
300400081030           LdvTxt = ds_Fed;
300500081030
300600101221         ENDFOR;
300700081030
300800081030       ENDSR;
300900081030
301000081030       //--------------------------------------------------------------
301100130716       //?Riempimento del box "Promemoria per l'autotrasportore"       ?
301200081030       //--------------------------------------------------------------
301300130716       BEGSR  sr_LdVbase01;
301400081030
301500120524         // -?Impostazione del modulo con Box aggiuntivo (14 rec. in +)?
301600131203         //// ·?Modulo Box aggiuntivo con logo Bartolini?
301700131203         //TemplateName = 'ldvbase01 ';
301800131203         //// ·?Modulo Box aggiuntivo con logo Bartolini/BRT?
301900131203         //TemplateName = 'ldvbase11 ';
302000120524         // ·?Modulo Box aggiuntivo con logo BRT?
302100120524         TemplateName = 'ldvbase21 ';
302200110517
302300081103         w_Row  = c_Row_B;
302400081030
302500101221         // -?Intestazione box "Promemoria per l'autotrasportatore"?
302600131206         //  ?(NON LA SI FA PIÙ)?
302700131206         //exsr  sr_IntestBox;
302800131206
302900131206
303000131206         // -?Posizionamento sulla 36ª riga?
303100131206         //  ?(Il ciclo "FOR" precedente dovrebbe aver già posizionato?
303200131206         //  ? sulla 37ª riga)?
303300131206         wx = 36;
303400131206
303500131206         // -?Se reperito il 1° PASSAGGIO (c/o destinatario):?
303600131206         IF  $1o_Passaggio;
303700131206
303800131206           test(de) EVBdev;
303900131206           if %error;
304000131206             clear w0080;
304100131206           else;
304200131206             Data_Eur = %date( %char(EVBdev) : *iso0 );
304300131206             w0080    = %dec(Data_Eur);
304400131206           endif;
304500131206
304600131206           // -?37ª riga?
304700131206           wx += 1;
304800131206           %occur(LdV) = wx;
304900131206           LdvPrg      = wx;
305000131206           Ppma    = 'Primo passaggio del '
305100131206                   + %editc( w0080 : 'Y' )
305200131206                   + ' effettuato alle '
305300131206                   + %editw( EVBhev: '  :  ' );
305400131206           LdvTxt  = ds_37;
305500131206
305600131206           // -?38ª riga: VUOTA (separatrice)?
305700131206           wx += 1;
305800131206           %occur(LdV) = wx;
305900131206           LdvPrg      = wx;
306000131206           clear  LdvTxt;
306100131206
306200131206         ENDIF;
306300131206
306400081030
306500131206         // -?Se reperite CONSEGNE PARZIALI PRECEDENTI:?
306600131206         IF  $ConsParz = *on;
306700131206
306800131206           // -?Impostazione segnalazione in output?
306900131206           xx = 1;
307000131206           DoW  (xx <= %elem($CP)   and   $CP(xx) < *all'9');
307100131206
307200131206             Elem_CP = $CP(xx);
307300131206             test(de) WCPdcm;
307400131206             if %error;
307500131206               clear w0080;
307600131206             else;
307700131206               Data_Eur = %date( %char(WCPdcm) : *iso0 );
307800131206               w0080    = %dec(Data_Eur);
307900131206             endif;
308000131206
308100131206             // -?37ª o 39ª riga & successive?
308200131206             wx += 1;
308300131206             %occur(LdV) = wx;
308400131206             LdvPrg      = wx;
308500131206             Ppma    = 'Consegnati colli '
308600131206                     + %editc(WCPncl:'Z')
308700131206                     + ' in data '
308800140304                     + %editc(w0080:'Y');
308900140304             if  WCPocm > *zero;
309000140304               Ppma    = %trimr( Ppma )
309100140304                       + ' alle ore '
309200140304                       + %editw( WCPocm : '0 :  ' );
309300140304             endif;
309400140304             if  WCPfir <> *blank;
309500140304               Ppma    = %trimr( Ppma )
309600140303                     + ' - Firmatario: '
309700140303                     + WCPfir;
309800140304             endif;
309900131206             LdvTxt  = ds_37;
310000131206
310100131206             xx += 1;
310200131206
310300131206           EndDo;
310400131206
310500131206         ENDIF;
310600081030
310700081030       ENDSR;
310800080801
310900080801       //--------------------------------------------------------------
311000130716       //?Intestazione del box "Promemoria per l'autotrasportore"      ?
311100080801       //--------------------------------------------------------------
311200131206       //BEGSR  sr_IntestBox;
311300131206       //
311400131206       //  // 37ª riga
311500131206       //  //wx += 1;         (già incrementato dal ciclo "FOR" precedente)?
311600131206       //  %occur(LdV) = wx;
311700131206       //  LdvPrg      = wx;
311800131206       //  Ppma    = '                             '
311900131206       //          + c_PMxA;
312000131206       //  LdvTxt  = ds_37;
312100131206       //
312200131206       //  // -?38ª riga?
312300131206       //  wx += 1;
312400131206       //  %occur(LdV) = wx;
312500131206       //  LdvPrg      = wx;
312600131206       //  Ppma    = *all'=';
312700131206       //  LdvTxt  = ds_37;
312800131206       //
312900131206       //ENDSR;
313000101221
313100080430      /end-free
313200081030      *
313300081030      *---------------------------------------------------------------*
313400081030      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
313500081030      *---------------------------------------------------------------*
313600081030     c     CarAWB_Arx    BEGSR
313700130215      *
313800130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
313900130215      *   comunque una bolla mamma
314000130215     c                   exsr      sr_Mamma_FedEx
314100081030      *
314200081030      * Posizionamento
314300130215if  2c                   if        $Mamma = *off   and
314400130215     c                             $Mamma_FedEx = *off
314500081030     c     KLDVnsc       setgt     FNART000
314600081030x   2c                   else
314700081030     c     KLBLnsc       setgt     FNART000
314800081030e   2c                   endif
314900081030      *
315000081030      * Ciclo di lettura
315100081030do  1c                   DO        *hival
315200081030      *
315300130215if  2c                   if        $Mamma = *off   and
315400130215     c                             $Mamma_FedEx = *off
315500081030     c     KLDV          reade     FNART000
315600081030x   2c                   else
315700081030     c     KLBL          reade     FNART000
315800081030e   2c                   endif
315900081030if  2c                   if        %eof(FNART01L)
316000081030     c                   leave
316100081030e   2c                   endif
316200081030      * - per ogni segnacollo cerco l'AWB
316300081030     c     K05ARS01A     chain     FIARS000
316400081030if  2c                   if            %found(FIARS01L)
316500081030     c                             and %subst(ARSnot:1:1) >= *zeros
316600081030     c                   eval      wAWB = %subst(ARSnot:1:12)
316700081030     c                   eval      wNSC = ARSnsc
316800081030     c                   leave
316900081030e   2c                   endif
317000081030      *
317100081030e   1c                   ENDDO
317200081030      *
317300081030     c                   ENDSR
317400130215      *
317500130215      /free
317600130215
317700130215       //--------------------------------------------------------------
317800130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
317900130215       //--------------------------------------------------------------
318000130215       BEGSR  sr_Mamma_FedEx;
318100130215
318200130924         //$Mamma_FedEx = *off;         ?(già così)?
318300130215
318400130924         If  Not $Mamma;
318500130215
318600130215           // -?Verifica se la spedizione ha comunque mamma?
318700130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
318800130215           if  not %open(FNLBL01L);
318900130215             open  FNLBL01L;
319000130215           endif;
319100130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
319200130215
319300130215           $Mamma_FedEx = %found(FNLBL01L);
319400130215
319500130215         EndIf;
319600130215
319700130215       ENDSR;
319800130215
319900130215      /end-free
320000131008      *
320100131008      *---------------------------------------------------------------*
320200131008      *?Prepara elenco SegnaColli Non Sequenziali (dett.) - MAX 2 -  ?*
320300131008      *?  da FNART (arrivo)                                          ?*
320400131008      *---------------------------------------------------------------*
320500131008     c     ListaScNSd    BEGSR
320600131008      *
320700131008      * SE ci sono solo 2 segnacolli non sequenziali:
320800131008      *   staranno al posto del range - basta non indicare
320900131008      *   "NON SEQUENZIALI"
321000131008      *
321100131008if  1c                   if        not %open(FNART01L)
321200131008     c                   open(e)   FNART01L
321300131008      /free
321400131008           if  %error;
321500131008             exsr  sr_ErrOpenFile;
321600131008           endif;
321700131008      /end-free
321800131008e   1c                   endif
321900131008      *
322000131008      * memorizzo l'ultimo numero di segnacollo
322100131008if  1c                   if        $Mamma = *off
322200131008     c     KLDV          setgt     FNART000
322300131008     c     KLDV          readpe    FNART000
322400131008x   1c                   else
322500131008     c     KLBL          setgt     FNART000
322600131008     c     KLBL          readpe    FNART000
322700131008e   1c                   endif
322800131008if  1c                   if        NOT %eof(FNART01L)
322900131008     c                   eval      %subst(Pnsx : 8) = ' , '
323000131008     c                                              + %editc(ARTnsc : 'Z')
323100131008e   1c                   endif
323200131008      *
323300131008      * memorizzo il primo numero di segnacollo
323400131008if  1c                   if        $Mamma = *off
323500131008     c     KLDV          setll     FNART000
323600131008     c     KLDV          reade     FNART000
323700131008x   1c                   else
323800131008     c     KLBL          setll     FNART000
323900131008     c     KLBL          reade     FNART000
324000131008e   1c                   endif
324100131008if  1c                   if        NOT %eof(FNART01L)
324200131008     c                   eval      %subst(Pnsx : 1 : 7) =
324300131008     c                                 %editc(ARTnsc : 'Z')
324400131008e   1c                   endif
324500131008      *
324600131008     c                   ENDSR
324700030515      *
324800030515      *---------------------------------------------------------------*
324900050217      *?Carico LDV da FNARB00F                                       ?*
325000030515      *---------------------------------------------------------------*
325100110208     c     sr_CarARB     BEGSR
325200030515      *
325300110202      /free
325400110202
325500110202         //____________________________________________________________
325600110202         //? RISULTATO è impostato di default a "OK" => creo immagine ?
325700110202         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
325800110202
325900110202         // »? STEP 1 ?«
326000131203         //  ?Verifica se presenti forzature per la distinta           ?
326100131203         //  ?(vedi file FIIRD00F)                                     ?
326200131203
326300131203         //?-?Verifica se distinta in FIIRD10F                           ?
326400131203         //  ?e se elaborabile con (IRDTPELAB = *blank) o senza controlli?
326500130716         chain  %kds(k01fiird11)  FIIRD000;
326600130313
326700130716         If  %found(FIIRD11L)  and  IRDtpElab = *blank;
326800131203           // -?SE rileva errori =>?
326900131203           // -?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
327000130716           exsr  sr_STEP_1_CtrlTASFLO;
327100130716         EndIf;
327200131203
327300131203         // »? STEP 2 ?«
327400131203         //  ?Verifica se spedizione elaborabile                       ?
327500131203         //  ?(controlli su distinta e spedizione nel file FIILV00F)   ?
327600131203         //  ?- SE distinta SENZA forzature in FIIRD00F -              ?
327700110202
327800110204         // -?Reperimento dati distinta (FIDST)?
327900110228         clear  dDstflr;
328000110202         k_DSTnpg = 4;
328100110202         k_DSTnfv = ds_Distinta.p_Ndc;
328200110202         k_DSTfgs = ds_Distinta.p_Fgs;
328300110202         chain  %kds(k03fidst01)  FIDST000;
328400110228         if  %found(FIDST01L);
328500110228           dDSTflr = DSTflr;
328600110228         endif;
328700110202
328800110202         // -?Controllo distinta:?
328900130920         //?·?(SE Distinta NON trovata in FIIRD10F per recupero immagini?
329000130924         //  ?  & NON è una spedizione FORZATA PER TEST)                ?
329100131016         //  ?SE distinta NON trovata in FIDST01L: registra LOG e prosegue?
329200131016         //  ?SE distinta NON chiusa:?
329300131016         //  ?"risponde" ed esce dalla s.p. (direttamente dalla subr.)?
329400130716         If  Not %found(FIIRD11L)  and
329500130716             $Test = *blank;
329600131203           exsr  sr_STEP_2_CtrlDISTINTA;
329700130716         EndIf;
329800110202
329900110202
330000110202         // -?Reperimento dati Immagine LdV (FIILV)?
330100110208         clear  ILVcol;
330200110309         clear  wCountRec01;
330300110202         chain(e)  %kds(k06fiilv01)  FIILV000;
330400130313
330500130716         //?·?DISTINTA in FIIRD10F => recupero immagine (senza ctrl)?
330600131016         //?·?DISTINTA NON in FIIRD10F => controlli da eseguire     ?
330700131016         //  ?SE trovati errori:?
330800131016         //  ?"risponde" ed eventualmente esce dalla s.p. (direttamente dalla subr.)?
330900130716         If  Not  %found(FIIRD11L);
331000131203           exsr  sr_STEP_2_CtrlFIILV;
331100130716         EndIf;
331200130313
331300131203         // »? STEP 3 ?«
331400131203         //  ?Controllo spedizione                                     ?
331500131203         //  ?(da eseguire comunque)                                   ?
331600110202
331700131203         // -?Reperimento dati Spedizione (FNARB)?
331800110207         chain  KLDV  FNARB000;
331900101220
332000110202         // -?Spedizione non trovata?
332100101221         if  NOT %found(FNARB01L);
332200130924           // ·?Spedizione non trovata => ERRORE: non rielaborare immagine?
332300131203           //  ?"risponde" ed esce dalla s.p.?
332400110204           Risultato = 'XX';
332500130924           //RtnOpCode = 'FNARB_MANC';
332600130924           //RtnEsito  = -1;
332700130924           // -?Registrazione log in FIILV, differenziando l'errore per?
332800130924           //  ?iter normale (25) o iter con forzatura (52)?
332900130924           if  Not  %found(FIIRD11L);
333000130313             wCod_Log = '25';
333100130716           else;
333200130716             wCod_Log = '52';
333300130716           endif;
333400110208           exsr  sr_WrtFIILV;
333500101221           // -?Uscita dalla s.p.?
333600110204           exsr  sr_End;
333700101221         endif;
333800101221
333900101221
334000110202         // -?Registrazione record per spedizione "OK" (root "buono")?
334100110218         If  wCod_Log = *blank;
334200130206           RootFolder = §ILVimgOk;
334300110203           wCod_Log = '01';
334400110202           exsr  sr_WrtFIILV;
334500110218         EndIf;
334600090121
334700080523      /end-free
334800070514      *
334900070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
335000070514      *   il reperimento dell'eventuale bolla mamma...
335100070514     c                   eval      wB0ntc =  ARBntc
335200120524     c                   eval      wB0dcp =  ARBdcp
335300120524     c                   eval      wB0tmc =  ARBtmc
335400120524     c                   move      ARBpdc        Wpdc4a
335500080523     c                   eval      wB0dcm =  ARBdcm
335600121018     c                   eval      wB0piv =  ARBcpi
335700030515      *
335800030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
335900030515     c                   exsr      sr_DS_ARB
336000030515      *
336100070308      * Decodifico filiale di Arrivo
336200131203     c                   exsr      sr_DecLNA
336300030515      *
336400030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
336500030515      *    "generiche" (3A, TB, 5E)
336600030515     c                   exsr      sr_DS_TAB
336700030515      *
336800030515      * Ultimi dati mancanti:
336900030515      * - Cod. cliente mittente
337000030515if  1c                   If        ARBccm <> *zeros
337100030515     c                   movel     ARBccm        DB0ccm
337200030515x   1c                   Else
337300030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
337400030515     c                   movel     ARBksc        DB0ccm
337500030515e   2c                   endif
337600030515e   1c                   EndIf
337700030515      * - Cod. cliente destinatario
337800030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
337900030515     c                   movel     ARBksc        DB0ccd
338000030515e   1c                   endif
338100030515      * - Se c'è 2ª bolla imposto LNA9999
338200030515if  1c                   if        DB0tb2 <> *blanks
338300030515     c                   eval      DB0ccd = *all'9'
338400030515     c                   movel     ARBlna        DB0ccd
338500030515e   1c                   endif
338600120524      * - Reperimento Data spedizione
338700120524if  1c                   if        DB0mgs <> *zeros
338800120524     c                   z-add     DB0mgs        Wdsp
338900120524     c                   movel     DB0aas        Wdsp
339000120524x   1c                   else
339100120524      *   prendo la data del giorno se non c'è la data spedizione
339200120524     c                   time                    w0140
339300120524     c                   move      w0140         w0080
339400120524     c     *eur          movel     w0080         Data_EUR
339500120524     c     *iso          movel     Data_EUR      Wdsp
339600120524e   1c                   endif
339700030515      *
339800061030      * 3ª impostaz. campi della DS: importi da calcolare
339900061031     c                   exsr      sr_DS_IMPO
340000030515      *
340100080505     c                   ENDSR
340200080724      /free
340300130716
340400130716       //--------------------------------------------------------------
340500130716       //?»  STEP 1  «                                                 ?
340600130716       //?Distinta in FIIRD10F e senza forzature (IRDTPLAB = *blank):  ?
340700130716       //?verifica se immagine già generata (DTASFLO.§FLOIML <> *blank)?
340800130716       //--------------------------------------------------------------
340900130716       BEGSR  sr_STEP_1_CtrlTASFLO;
341000130716
341100130716         // -?Reperimento spedizione da TITAS per verificarne lo stato?
341200131016         //  ?della relativa immagine.  Se già presente immagine:?
341300131016         //  ?"risponde" con codice errore 51 e "non elaborare"?
341400131016         //  ?ed esce dalla s.p.?
341500130716         if  %subst( wSysName : 1 : 6 ) = 'SETRAS';
341600130716           wLibFileTITAS = 'GAITRAGRU/TITAS30C';
341700130716         else;
341800130716           wLibFileTITAS = 'GAITRAGRPS/TITAS30C';
341900130716         endif;
342000130716         if  NOT  %open(TITAS30C);
342100130716           open(e)  TITAS30C;
342200130716           if  %error;
342300130716             exsr  sr_ErrOpenFile;
342400130716           endif;
342500130716         endif;
342600130716
342700130716         chain  %kds(k05titas30 : 4)  TITAS30C;
342800130716
342900130716         If  %found(TITAS30C)            and
343000130716             TASdcm > *zero              and
343100130716             TASndc = ds_Distinta.p_Ndc  and
343200130716             %int( TASpdc / 10000 ) = ds_Distinta.p_Fgs;
343300130716
343400130716           dTASflo = TASflo;
343500130716
343600130716           if  dTASflo.§FLOiml <> *blank;
343700130716             Risultato = 'XX';
343800130924             //RtnOpCode = 'GIÀ_IMMAG ';
343900130924             //RtnEsito  = -1;
344000130716             // -?Registrazione log in FIILV?
344100130716             wCod_Log = '51';
344200130716             exsr  sr_WrtFIILV;
344300130716             // -?Uscita dalla s.p.?
344400130716             exsr  sr_End;
344500130716           endif;
344600130716
344700130716         EndIf;
344800130716
344900130716       ENDSR;
345000130716
345100130716       //--------------------------------------------------------------
345200131203       //?»  STEP 2  «                                                 ?
345300131203       //?Distinta NON in FIIRD10F:?controlli sulla distinta.          ?
345400130716       //--------------------------------------------------------------
345500131203       BEGSR  sr_STEP_2_CtrlDISTINTA;
345600130716
345700130716         Select;
345800130716
345900130716           // ·?DISTINTA non trovata => ANOMALIA?
346000130716           When  NOT  %found(FIDST01L)  or
346100130716                 DSTatb <> *blank;
346200130924             // -?Registrazione log in FIILV - se non già fatto?
346300130716             if  wCod_Log = *blank;
346400130716               wCod_Log = '22';
346500130716               exsr  sr_WrtFIILV;
346600130716             endif;
346700130716
346800131016           // ·?DISTINTA non chiusa => ripetere la richiesta?
346900130716           When  DSTfcf = *blank;
347000130716             Risultato  = 'NO';
347100130924             //RtnOpCode  = 'DIST_APERT';
347200130716             //RtnEsito   = -1;
347300130716             // -?Uscita dalla s.p.?
347400130716             exsr  sr_End;
347500130716
347600130716         EndSl;
347700130716
347800130716       ENDSR;
347900130716
348000130716       //--------------------------------------------------------------
348100131203       //?»  STEP 2  «                                                 ?
348200131203       //?Distinta NON in FIIRD10F:?controlli sul/i record in FIILV00F.?
348300130716       //--------------------------------------------------------------
348400131203       BEGSR  sr_STEP_2_CtrlFIILV;
348500130716
348600130716         Select;
348700130716
348800130716           // ·?Rec. IMMAGINE LdV allocato => ripetere la richiesta?
348900130716           When  %error;
349000130716             Risultato  = 'NO';
349100130924             //RtnOpCode  = 'FIILV_ALLC';
349200130716             //RtnEsito   = -1;
349300130716             // -?Uscita dalla s.p.?
349400130716             exsr  sr_End;
349500130716
349600130716           // ·?Rec. IMMAGINE LdV non trovato?
349700130716           When  NOT  %found(FIILV01L);
349800130716             select;
349900130716               // ·?DISTINTA senza scarico a PDA delle spedizioni?
350000130716               //  ?=> cancellare immagine (senza LOG)?
350100130716               when  DSTpda <> 'C'   and   DSTpda <> 'E';
350200130716                 Risultato  = 'XX';
350300130924                 //RtnOpCode  = 'DIST_NOPDA';
350400130716                 //RtnEsito   = -1;
350500130716                 // -?Uscita dalla s.p.?
350600130716                 exsr  sr_End;
350700130716               // ·?DISTINTA in test consegne?
350800130716               //  ?=> cancellare immagine (senza LOG)?
350900130716               when  §DSTtstPda = 'C'   or   §DSTtstPda = 'E';
351000130716                 Risultato  = 'XX';
351100130924                 //RtnOpCode  = 'DIST_TEST ';
351200130716                 //RtnEsito   = -1;
351300130716                 // -?Uscita dalla s.p.?
351400130716                 exsr  sr_End;
351500130716               // ·?Altrimenti: ANOMALIA?
351600130924               //  ?Registrazione log in FIILV - se non già fatto?
351700130716               other;
351800130716                 if  wCod_Log = *blank;
351900130716                   wCod_Log = '23';
352000130716                   exsr  sr_WrtFIILV;
352100130716                 endif;
352200130716             endsl;
352300130716
352400130716           // ·?Rec. IMMAGINE LdV già elaborato?
352500130716           //  ?=> ricerca record NON elaborato (ILVCOL = "00")?
352600130716           When  ILVcol <> '00';
352700130716             exsr  sr_CercaLog00;
352800130716             // ·?Rec. IMMAGINE LdV non trovato (né "00" né "01")?
352900130924             //  ?Registrazione log in FIILV - se non già fatto?
353000130716             if  ILVcol <> '00'  and  wCountRec01 = *zero
353100130716                                 and  wCod_Log    = *blank;
353200130716               wCod_Log = '23';
353300130716               exsr  sr_WrtFIILV;
353400130716             endif;
353500130716
353600130716         EndSl;
353700130716
353800130716
353900130716         // -?Verifica SE trovato record da elaborare per la sped/dist?
354000130716         $FIILV = (ILVcol = '00');
354100130716
354200130716         // -?Impostazione struttura dati del campo ILVLIBERO?
354300130716         dILVlibero = ILVlibero;
354400130716
354500130716
354600130716         Select;
354700130716
354800130716           // -?Spedizione non consegnata in distinta?
354900130716           When  ILVnoCDst = 'S';
355000130924             // -?Registrazione log in FIILV - se non già fatto?
355100130716             if  wCod_Log = *blank;
355200130716               wCod_Log = '08';
355300130716               exsr  sr_WrtFIILV;
355400130716             endif;
355500130716
355600130716           // -?LdV da scannerizzare?
355700130716           When  ILVscan = *blank;
355800130924             // -?Registrazione log in FIILV - se non già fatto?
355900130716             if  wCod_Log = *blank;
356000130716               wCod_Log = '09';
356100130716               exsr  sr_WrtFIILV;
356200130716             endif;
356300130716
356400130716         EndSl;
356500130716
356600130716       ENDSR;
356700130716
356800130716       //--------------------------------------------------------------
356900130716       //?Ricerca record IMMAGINE LDV NON elaborato (ILVCOL = "00")    ?
357000130716       //--------------------------------------------------------------
357100130716       BEGSR  sr_CercaLog00;
357200130716
357300130716         //?N.B.?Il 1° rec. per spedizione/distinta è già stato letto?
357400130716
357500130716         DoU  ILVcol = '00';
357600130716
357700130716           // ·?Rec. Immagine LdV già elaborato ("01")?
357800130716           if  ILVcol = '01';
357900130716             wCountRec01 += 1;
358000130716           endif;
358100130716
358200130716           reade(e)  %kds(k06fiilv01)  FIILV000;
358300130716
358400130716           select;
358500130716
358600130716             // ·?Rec. Immagine LdV allocato => ripetere la richiesta?
358700130716             when  %error;
358800130716               Risultato  = 'NO';
358900130924               //RtnOpCode  = 'FIILV_ALLC';
359000130716               //RtnEsito   = -1;
359100130716               // -?Uscita dalla s.p.?
359200130716               exsr  sr_End;
359300130716
359400130716             // ·?Solo rec. Immagine LdV già elaborati?
359500130716             //  ?=> scrittura nuovo?
359600130716             when  %eof(FIILV01L);
359700130716               clear  ILVcol;
359800130716               leave;
359900130716
360000130716             // ·?Già reperiti 100 rec. per le stesse sped./dist.?
360100130716             //  ?(questo è in realtà il 101°)?
360200131016             //  ?=> NON ripetere la richiesta (senza LOG)?
360300130716             when  wCountRec01 >= 100;
360400130716               Risultato  = 'XX';
360500130924               //RtnOpCode  = 'SPEDGIÀ100';
360600130716               //RtnEsito   = -1;
360700130716               // -?Invio e-mail?
360800130716               exsr  sr_SendMAIL;
360900130716               // -?Uscita dalla s.p.?
361000130716               exsr  sr_End;
361100130716
361200130716             // ·?Rec. Immagine LdV già elaborato con data/ora?
361300130716             //  ?creazione immagine sul PDA successive all'attuale?
361400131016             //  ?=> NON ripetere la richiesta (senza LOG)?
361500130716             //when  DataOraCrea <= ILVprmTimP;
361600130716             when  DataOraCrea < ILVprmTimP;
361700130716               Risultato  = 'XX';
361800130924               //RtnOpCode  = 'SPED_GIÀEL';
361900130716               //RtnEsito   = -1;
362000130716               // -?Uscita dalla s.p.?
362100130716               exsr  sr_End;
362200130716
362300130716             // ·?Rec. Immagine LdV già elaborato con data/ora?
362400130716             //  ?creazione immagine sul PDA identica all'attuale?
362500130716             //  ?=> riprovare dopo 10 minuti (senza LOG)?
362600130716             when  DataOraCrea = ILVprmTimP  and
362700130716                   ILVtimA > %int( %subst( %char( %dec(
362800130716                                   %timestamp() - %minutes(10) ) )
362900130716                                           : 1 : 14 ) );
363000130716               Risultato  = 'NO';
363100130924               //RtnOpCode  = '..TRA10MIN';
363200130716               //RtnEsito   = -1;
363300130716               // -?Uscita dalla s.p.?
363400130716               exsr  sr_End;
363500130716
363600130716           endsl;
363700130716
363800130716         EndDo;
363900130716
364000130716       ENDSR;
364100101221
364200080724       //--------------------------------------------------------------
364300130716       //?Registrazione record nel work-file FIILV00F                  ?
364400080724       //--------------------------------------------------------------
364500110207       BEGSR  sr_WrtFIILV;
364600110202
364700110202         // -?Segnalazione anomalia rilevata per la spedizione?
364800110204         if  Not  $FIILV;
364900110202           clear  FIILV000;
365000110202           ILVfgs = ds_Distinta.p_Fgs;
365100110202           ILVndc = ds_Distinta.p_Ndc;
365200110202           ILVaas = DB0aas;
365300110202           ILVlnp = DB0lnp;
365400110202           ILVnrs = DB0nrs;
365500110202           ILVnsp = DB0nsp;
365600110202         endif;
365700110202
365800110207         ILVcol     = wCod_Log;
365900110202         xx = %lookup( wCod_Log : $CoL );
366000110202         if  xx > *zero;
366100110202           ILVnot   = $DeL(xx);
366200110202         endif;
366300110202         ILVprmSpe  = Spedizione;
366400110202         ILVprmDst  = Distinta;
366500110202         ILVprmTimp = DataOraCrea;
366600110202         ILVrisult  = Risultato;
366700110202         ILVtimA    = %int( %subst( %char( %dec( %timestamp() ) ) :1 :14 ) );
366800170707
366900170707         dILVlibero.§ILVapl = 'G';
367000170707
367100170707         ILVlibero = dILVlibero;
367200130411
367300130411
367400131203         // -? Spedizioni FORZATE PER TEST => NON scrive il rec.       ?
367500130411         IF  $Test <> *blank;
367600130411           leavesr;
367700130411         ENDIF;
367800130411
367900090210
368000110923
368100131203         // -?S.P. richiamata per stampa cartacea => NON scrive il rec.?
368200110923         If  SDSprm > 11;
368300110923           leavesr;
368400110923         EndIf;
368500110923
368600110923
368700110204         if  $FIILV;
368800110202           //_______________
368900110202           update  FIILV000;
369000110202           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
369100110202         else;
369200110204           //_______________
369300110204           write   FIILV000;
369400110204           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
369500110202         endif;
369600080724
369700080724       ENDSR;
369800130227
369900130227       //--------------------------------------------------------------
370000130716       //?Invio e-mail per avvisare degli oltre 100 rec. con cod. LOG  ?
370100130716       //?   "01" per la stessa spedizione in FIILV.                   ?
370200130227       //--------------------------------------------------------------
370300130227       BEGSR  sr_SendMAIL;
370400130227
370500130227         // -?Reperimento tab. "MRA"?
370600130227         reset  dMRAdan;
370700130227         reset  TIBS02ds;
370800130227         T02cod = 'MRA';
370900130227         T02ke1 = SDSpgm;
371000130227         T02ke2 = *blank;
371100130227         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
371200130227         if  T02err = *blank;
371300130227           dMRAdan = T02uni;
371400130227         endif;
371500130227
371600130227         // -?Override ai file di stampa?
371700130227         //  ?(per impostarvi i dati per l'invio via e-mail)?
371800130227         exsr sr_OvrPrtF;
371900130227
372000130227         // -?Stampa segnalazione?
372100130227         except  EmlTxt;
372200130227         P_Fgs2 = ds_Distinta.p_Fgs;
372300130227         P_Ndc2 = ds_Distinta.p_Ndc;
372400130227         except  EmlDet;
372500130227         except  EmlEnd;
372600130227
372700130227         // -?Cancellazione Override ai file di stampa?
372800130227         exsr sr_DeleteOvr;
372900130227
373000130227       ENDSR;
373100130227
373200130227       //--------------------------------------------------------------
373300130716       //?Override al file di stampa per impostarvi i dati per         ?
373400130716       //?  l'invio via e-mail.                                        ?
373500130227       //--------------------------------------------------------------
373600130227       BEGSR  sr_OvrPrtF;
373700130227
373800130227         reset  TRTCM1ds;
373900130227         if  §MRAdmitt <> *blank;
374000130227           §CM1mitt = %trim(§MRAdmitt);
374100130227         endif;
374200130227         §CM1dst  = %trim(§MRAddest);
374300130227         §CM1tips = §MRAdreg;
374400130227         //§CM1var  = '*OBJM*' + §MRAddes;
374500130227         §CM1idp  = §MRAdidpro;
374600130227
374700130227         // -?Override al "corpo e-mail"?
374800130227         if  §MRAdreg <> *blank;
374900130227           Qcmd = c_CmdOvrPrtF
375000130227                + ' outq(' + %trim(§MRAdoutqi) + ')'
375100130227                + ' usrdfndta(''' + TRTCM1ds + ''')';
375200130227         else;
375300130227           Qcmd = c_CmdOvrPrtF;
375400130227         endif;
375500130227
375600130227         exsr  sr_ExecCmd;
375700130227
375800130227         //if  Qusei <> *blank;
375900130227         //  ...;
376000130227         //endif;
376100130227
376200130227         // -?Apertura file di stampa?
376300130227         open  PrtEmail;
376400130227
376500130227       ENDSR;
376600130227
376700130227       //--------------------------------------------------------------
376800130227       //?Chiusura file di stampa e cancellazione override.            ?
376900130227       //--------------------------------------------------------------
377000130227       BEGSR  sr_DeleteOvr;
377100130227
377200130227         // -?Chiusura file di stampa?
377300130227         if  %open(PrtEMAIL);
377400130227           close  PrtEMAIL;
377500130227         endif;
377600130227
377700130227         // -?Cancellazione override al file di stampa?
377800130227         Qcmd = c_CmdDltOvr;
377900130227         exsr  sr_ExecCmd;
378000130227
378100130227       ENDSR;
378200130227
378300130227       //--------------------------------------------------------------
378400130227       //?Esecuzione del comando (già impostato).                      ?
378500130227       //--------------------------------------------------------------
378600130227       BEGSR  sr_ExecCmd;
378700130227
378800130227         clear Qcap0100;
378900130227         Qcabcsdh = *off;
379000130227         Qcapa    = *off;
379100130227         Qcacmdss = *off;
379200130227         Qcaerved = *allX'00';
379300130227
379400130227         clear Qusec;
379500130227         Qusbprv  = %size(Qusec);
379600130227
379700130227         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
379800130227                           %size(Qcap0100) : 'CPOP0100' : *omit :
379900130227                           0 : 0 : Qusec);
380000130227
380100130227         //if  Qusei <> *blank;
380200130227         //  ...;
380300130227         //endif;
380400130227
380500130227       ENDSR;
380600101221
380700080724      /end-free
380800030515      *
380900030515      *---------------------------------------------------------------*
381000050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
381100030515      *---------------------------------------------------------------*
381200030515     c     sr_DS_ARB     BEGSR
381300030515      *
381400030515     c                   eval      DB0cbo = ARBcbo
381500030515     c                   eval      DB0tsp = ARBtsp
381600030515     c                   eval      DB0mgs = ARBmgs
381700030515     c                   eval      DB0tc1 = ARBtc1
381800030515     c                   eval      DB0tc2 = ARBtc2
381900030515     c                   eval      DB0fns = ARBfns
382000030515     c                   eval      DB0rmn = ARBrmn
382100030515     c                   eval      DB0rma = ARBrma
382200030515     c                   eval      DB0ias = ARBias
382300030515     c                   eval      DB0vas = ARBvas
382400030515     c                   eval      DB0vmd = ARBvmd
382500030515     c                   eval      DB0vad = ARBvad
382600030515     c                   eval      DB0qft = ARBqft
382700030515     c                   eval      DB0lna = ARBlna
382800030515     c                   eval      DB0znc = ARBznc
382900030515     c                   eval      DB0rsm = ARBrsm
383000030515     c                   eval      DB0inm = ARBinm
383100030515     c                   eval      DB0cam = ARBcam
383200030515     c                   eval      DB0lom = ARBlom
383300030515     c                   eval      DB0prm = ARBprm
383400030515     c                   eval      DB0nzm = ARBnzm
383500030515     c                   eval      DB0fap = ARBfap
383600030515     c                   eval      DB0rmo = ARBrmo
383700030515     c                   eval      DB0rsd = ARBrsd
383800030515     c                   eval      DB0ind = ARBind
383900030515     c                   eval      DB0cad = ARBcad
384000030515     c                   eval      DB0lod = ARBlod
384100030515     c                   eval      DB0prd = ARBprd
384200030515     c                   eval      DB0nzd = ARBnzd
384300030515     c                   eval      DB0fin = ARBfin
384400030515     c                   eval      DB0gc1 = ARBgc1
384500030515     c                   eval      DB0gc2 = ARBgc2
384600030515     c                   eval      DB0nas = ARBnas
384700030515     c                   eval      DB0ncl = ARBncl
384800030515     c                   eval      DB0pkf = ARBpkf
384900030515     c                   eval      DB0fvf = ARBfvf
385000030515     c                   eval      DB0vlf = ARBvlf
385100030515     c                   eval      DB0fls = ARBfls
385200030515     c                   eval      DB0ncd = ARBncd
385300030515     c                   eval      DB0nca = ARBnca
385400030515     c                   eval      DB0xco = ARBxco
385500030515     c                   eval      DB0ffd = ARBffd
385600030515     c                   eval      DB0dcr = ARBdcr
385700030515     c                   eval      DB0hcr = ARBhcr
385800030515     c                   eval      DB0tcr = ARBtcr
385900030515     c                   eval      DB0fbr = ARBfbr
386000030515     c                   eval      DB0gga = ARBgga
386100030515     c                   eval      DB0gma = ARBgma
386200030515     c                   eval      DB0gva = ARBgva
386300030515     c                   move      ' '           DB0gva
386400030515     c                   clear                   DB0sop
386500030515     c                   eval      DB0ctr = ARBctr
386600030515     c                   clear                   DB0drt
386700030515     c                   clear                   DB0fpp
386800030515     c                   clear                   DB0nrt
386900030515     c                   clear                   DB0pdr
387000030515      *
387100030515      * Allega documento di trasporto
387200060524      * & Codice cliente da Assegnato
387300130924     c***?(già fatto:)?  clear                   dsBL4A
387400031029     c                   clear                   DB0bam
387500061107     c                   exsr      RepAR4_A
387600060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
387700031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
387800030515     c                   movel     'S'           DB0bam
387900060505     c                   endif
388000030515      *
388100030515      * Destinatario: vedo se esiste 2ª ragione sociale
388200061107     c                   clear                   DB0rd2
388300061107     c                   exsr      RepAR4_D
388400030515      *
388500030515      * Note
388600061107     c                   clear                   DB0nt1
388700061107     c                   clear                   DB0nt2
388800061107     c                   exsr      RepAR4_89
388900030515      *
389000030515     c                   ENDSR
389100030515      *
389200030515      *---------------------------------------------------------------*
389300050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
389400050217      *?   "generiche" (non da "1A")                                 ?*
389500030515      *---------------------------------------------------------------*
389600030515     c     sr_DS_TAB     BEGSR
389700030515      *
389800030515      * Carico le tabelle "generiche" (3A, TB, 5E)
389900030515     c                   exsr      CarTAB
390000030515      *
390100030515      * Codici bolla
390200030515     c                   eval      DB0tb1 = §3Atb1
390300030515     c                   eval      DB0tb2 = §3Atb2
390400030515     c                   eval      DB0dcb = §3Ades
390500030515      * Tipo servizio
390600030515if  1c                   if        DB0dts = *blanks
390700030515     c                   eval      DB0dts = §5Edes
390800030515     c                   eval      DB0fst = §5Efst
390900030515e   1c                   endif
391000030515      * Consegne particolari
391100030515if  1c                   if            DB0tc1 <> *blanks
391200030515     c                             and DB0dc1 =  *blanks
391300030515     c                   movel(p)  DB0tc1        TBLkey
391400030515     c                   exsr      Tab_1P
391500030515     c                   eval      DB0dc1 = §1Pdes
391600030515e   1c                   endif
391700030515if  1c                   if            DB0tc2 <> *blanks
391800030515     c                             and DB0dc2 =  *blanks
391900030515     c                   movel(p)  DB0tc2        TBLkey
392000030515     c                   exsr      Tab_1P
392100030515     c                   eval      DB0dc2 = §1Pdes
392200030515e   1c                   endif
392300030515      * Esenzione IVA
392400030515if  1c                   if            DB0cei <> *blanks
392500030515     c                             and DB0dce =  *blanks
392600030515     c                   eval      DB0dce =  §EIdes
392700030515e   1c                   endif
392800030515      *
392900030515     c                   ENDSR
393000060524      *
393100060524      *---------------------------------------------------------------*
393200060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
393300061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
393400060524      *---------------------------------------------------------------*
393500060524     c     RepAR4_A      BEGSR
393600060524      *
393700130924     c***?(già fatto:)?  clear                   dsBL4A
393800061107      *
393900060524     c                   eval      Wtrc = 'A'
394000060524     c     KLDVtrc       chain     FIAR4000
394100060524      *
394200060524if  1c                   if        %found(FIAR401L)
394300060524     c                   movel     AR4Not        dsBL4A
394400060524e   1c                   endif
394500060524      *
394600060524     c                   ENDSR
394700061107      *
394800061107      *---------------------------------------------------------------*
394900061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
395000061107      *?(2ª ragione sociale del destinatario)                        ?*
395100061107      *---------------------------------------------------------------*
395200061107     c     RepAR4_D      BEGSR
395300061107      *
395400130924     c***?NON esiste:?   clear                   dsBL4D
395500130924     c***?(già fatto:)?  clear                   DB0rd2
395600061107      *
395700061107     c                   eval      Wtrc = 'D'
395800061107     c     KLDVtrc       chain     FIAR4000
395900061107      *
396000061107if  1c                   if        %found(FIAR401L)
396100061107     c                   movel     AR4not        DB0rd2
396200061107e   1c                   endif
396300061107      *
396400061107     c                   ENDSR
396500061107      *
396600060626      *---------------------------------------------------------------*
396700060626      *?Reperimento dati da FIAR400F x trk "I"                       ?*
396800061107      *?(dati DPD)                                                   ?*
396900060626      *---------------------------------------------------------------*
397000061030     c     RepAR4_I      BEGSR
397100061030      *
397200130924     c***?(già fatto:)?  clear                   dsBL4I
397300061107      *
397400061030     c                   eval      Wtrc = 'I'
397500061030     c     KLDVtrc       chain     FIAR4000
397600061030      *
397700061030if  1c                   if        %found(FIAR401L)
397800061030     c                   movel     AR4not        dsBL4I
397900060626e   1c                   endif
398000061030      *
398100061030     c                   ENDSR
398200071008      *
398300071015      *---------------------------------------------------------------*
398400071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
398500110204      *?(dati aggiuntivi per O.R.M. abbinato alla spedizione)        ?*
398600071015      *---------------------------------------------------------------*
398700071015     c     RepAR4_M      BEGSR
398800071015      *
398900130924     c***?(già fatto:)?  clear                   dsBL4M
399000071015      *
399100071015     c                   eval      Wtrc = 'M'
399200071015     c     KLDVtrc       chain     FIAR4000
399300071015      *
399400071015if  1c                   if        %found(FIAR401L)
399500071015     c                   movel     AR4Not        dsBL4M
399600071015e   1c                   endif
399700071015      *
399800071015     c                   ENDSR
399900121018      *
400000121018      *---------------------------------------------------------------*
400100121018      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
400200121018      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
400300121018      *---------------------------------------------------------------*
400400121018     c     RepAR4_Q      BEGSR
400500121018      *
400600130924     c***?(già fatto:)?  clear                   dTA4Q
400700130924     c***?(già fatto:)?  clear                   DB0cpi
400800121018      *
400900121018     c                   eval      Wtrc = 'Q'
401000121018     c     KLDVtrc       chain     FIAR4000
401100121018      *
401200121018if  1c                   if        %found(FIAR401L)
401300121018     c                   movel     AR4not        dTA4Q
401400121018e   1c                   endif
401500121018      *
401600121018     c                   ENDSR
401700080523      /free
401800080523
401900080523       //--------------------------------------------------------------
402000080523       //?Reperimento dati da FIAR400F x trk "1"                       ?
402100080523       //?(nominativo del destinatario)                                ?
402200080523       //--------------------------------------------------------------
402300080523       BEGSR  RepAR4_1;
402400080523
402500101222         //clear  dsBL41;        *** NON esiste ***?
402600080523
402700080523         Wtrc = '1';
402800080523         chain  KLDVtrc  FIAR4000;
402900080523
403000080523         if  NOT  %found(FIAR401L);
403100080523           clear  AR4not;
403200080523         endif;
403300080523
403400150430         if  DB0ccm = c_CliFedEx  or  DB0ccm = c_CliFedEx2;
403500081104           Pfedkcn = AR4not;
403600081104         else;
403700081104           Pkcn    = AR4not;
403800081104         endif;
403900080523
404000080523       ENDSR;
404100080523
404200080523      /end-free
404300061107      *
404400061107      *---------------------------------------------------------------*
404500061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
404600061107      *?(note)                                                       ?*
404700061107      *---------------------------------------------------------------*
404800061107     c     RepAR4_89     BEGSR
404900061107      *
405000130924     c***?NON esiste:?   clear                   dsBL48
405100130924     c***?NON usata:?    clear                   dsBL49
405200130924     c***?(già fatto:)?  clear                   DB0nt1
405300130924     c***?(già fatto:)?  clear                   DB0nt2
405400061107      *
405500061107     c                   eval      Wtrc = '8'
405600061107     c     KLDVtrc       chain     FIAR4000
405700061107if  1c                   if        %found(FIAR401L)
405800061107     c                   movel     AR4not        DB0nt1
405900061107e   1c                   endif
406000061107      *
406100061107     c                   eval      Wtrc = '9'
406200061107     c     KLDVtrc       chain     FIAR4000
406300061107if  1c                   if        %found(FIAR401L)
406400061107     c                   movel     AR4not        DB0nt2
406500061107e   1c                   endif
406600061107      *
406700061107     c                   ENDSR
406800030515      *
406900030515      *---------------------------------------------------------------*
407000050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
407100030515      *---------------------------------------------------------------*
407200030515     c     CarTAB        BEGSR
407300030515      *
407400030515      * Tab. "3A" - Codici Bolla
407500030515      * (NO se rimesso mittente)
407600030515     c                   clear                   ds3A
407700030515if  1c                   if        DB0ris <> 'M'
407800030515     c                   movel     '3A'          TBLcod
407900030515     c                   movel(p)  DB0cbo        TBLkey
408000030515     c     K03TAB        chain     TABEL
408100030515if  2c                   if        %found(TABEL00F)
408200030515     c                   movel     TBLuni        ds3A
408300030515e   2c                   endif
408400030515e   1c                   endif
408500070515     c                   eval      $Mamma =  (§3Abcm = 'S')
408600030515      *
408700030515      * Tab. "TB" - Tipi Bolla
408800030515      * (NO se rimesso mittente)
408900030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
409000030515      * se no non stampo i dati fattura
409100030515     c                   clear                   dsTB
409200030515if  1c                   if        DB0ris <> 'M'
409300030515     c                   movel     'TB'          TBLcod
409400121029if  2c                   if        §3Atb2 <> *blanks
409500030515     c                   movel(p)  §3Atb2        TBLkey
409600030515x   2c                   else
409700030515     c                   movel(p)  §3Atb1        TBLkey
409800030515e   2c                   endif
409900030515     c     K03TAB        chain     TABEL
410000030515if  2c                   if        %found(TABEL00F)
410100030515     c                   movel     TBLuni        dsTB
410200030515e   2c                   endif
410300030515e   1c                   endif
410400030515      *
410500030515      * Tab. "5E" - Tipi Servizio
410600030515     c                   clear                   ds5E
410700090514if  1c                   if        DB0tsp <> *blanks
410800030515     c                   movel     '5E'          TBLcod
410900030515     c                   movel(p)  DB0tsp        TBLkey
411000030515     c     K03TAB        chain     TABEL
411100030515if  2c                   if        %found(TABEL00F)
411200030515     c                   movel     TBLuni        ds5E
411300030515e   2c                   endif
411400030515e   1c                   endif
411500030515      *
411600030911      * Tab. "EI" - Esenzioni I.V.A.
411700030515     c                   clear                   dsEI
411800030911      * Tab. "1A" - Tipi incasso contrassegno
411900030911     c                   clear                   ds1A
412000030911      * Tab. "1P" - Consegne particolari
412100030911     c                   clear                   ds1P
412200030515      *
412300030515     c                   ENDSR
412400030911      *
412500030911      *---------------------------------------------------------------*
412600050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
412700030911      *---------------------------------------------------------------*
412800030911     c     Tab_EI        BEGSR
412900030911      *
413000030911     c                   clear                   dsEI
413100030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
413200030911     c                   movel     'EI'          TBLcod
413300030911     c                   movel(p)  DB0cei        TBLkey
413400030911     c     K03TAB        chain     TABEL
413500030911if  2c                   if        %found(TABEL00F)
413600030911     c                   movel     TBLuni        dsEI
413700030911e   2c                   endif
413800030911e   1c                   endif
413900030911     c                   eval      DB0dce =  §EIdes
414000030911      *
414100030911     c                   ENDSR
414200030515      *
414300030515      *---------------------------------------------------------------*
414400050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
414500030515      *---------------------------------------------------------------*
414600030515     c     Tab_1A        BEGSR
414700030515      *
414800030515     c                   clear                   ds1A
414900030515if  1c                   if        DB0cas <> *zeros
415000030515     c                   movel     '1A'          TBLcod
415100030515     c                   movel(p)  DB0tic        TBLkey
415200030515     c     K03TAB        chain     TABEL
415300030515if  2c                   if        %found(TABEL00F)
415400030515     c                   movel     TBLuni        ds1A
415500030515e   2c                   endif
415600030515e   1c                   endif
415700030515      *
415800030515      * Imposto i dati nella DS
415900030515     c                   eval      DB0dti = §1Ades
416000030515     c                   eval      DB0ucm = §1Aucm
416100030515     c                   eval      DB0ste = §1Aste
416200030515      *
416300030515     c                   ENDSR
416400030515      *
416500030515      *---------------------------------------------------------------*
416600050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
416700030515      *---------------------------------------------------------------*
416800030515     c     Tab_1P        BEGSR
416900030515      *
417000030515     c                   clear                   ds1P
417100030515     c                   movel     '1P'          TBLcod
417200130924     c***?già impostato:?movel(p)  DB0tc1/2      TBLkey
417300030515     c     K03TAB        chain     TABEL
417400030515if  1c                   if        %found(TABEL00F)
417500030515     c                   movel     TBLuni        ds1P
417600030515e   1c                   endif
417700030515      *
417800030515     c                   ENDSR
417900030515      *
418000030515      *---------------------------------------------------------------*
418100050217      *?Calcolo importi LDV                                          ?*
418200030515      *---------------------------------------------------------------*
418300061031     c     sr_DS_IMPO    BEGSR
418400030515      *
418500030515      * Vedo se esiste tassazione
418600030515      *  solo se tipo bolla contabilizzabile
418700061030if  1c                   IF        §TBfcb =  '1'
418800101213      *
418900061030     c                   eval      Wtrc   =  '1'
419000040607     c     KLDVtrc       chain     FIAR6000
419100101213      *
419200030515if  2c                   if        %found(FIAR601L)
419300030515     c                   movel     AR6div        DB0div
419400030515     c                   exsr      CtrDIV
419500030515      *
419600030515      * NON c'è 2ª bolla
419700101213if  3c                   if        DB0tb2 = *blanks
419800030515      *  Estremi fattura
419900030515     c                   z-add     AR6nft        DB0nft
420000030515     c                   z-add     AR6dft        DB0dft
420100130205     c                   z-add     AR6fiv        wB0fiv
420200101213      *  Se è assegnato => stampo tutta la tassaz.
420300101213if  4c                   if        %subst(DB0tb1:1:1) = 'A'
420400061030     c                   eval      Wtrc   =  '1'
420500030515     c                   exsr      RepTASSAZ
420600030515e   4c                   endif
420700030515e   3c                   endif
420800030515e   2c                   endif
420900030515      *
421000061030      * C'è 2ª bolla
421100101213if  2c                   IF        DB0tb2 <> *blanks
421200030515      *
421300061030     c                   eval      Wtrc   =  '2'
421400040607     c     KLDVtrc       chain     FIAR6000
421500030515      *
421600061030if  3c                   If        %found(FIAR601L)
421700030515     c                   movel     AR6div        DB0div
421800030515     c                   exsr      CtrDIV
421900030515     c                   movel     AR6cei        DB0cei
422000030515      *   Estremi fattura
422100030515     c                   z-add     AR6nft        DB0nft
422200030515     c                   z-add     AR6dft        DB0dft
422300130205     c                   z-add     AR6fiv        wB0fiv
422400030515if  4c                   if        AR6ksc <> *zeros
422500030515     c                   eval      DB0ccd =  AR6ksc
422600030515e   4c                   endif
422700061030     c                   eval      Wtrc   =  '2'
422800030515     c                   exsr      RepTASSAZ
422900061030e   3c                   EndIf
423000030515      *
423100030515e   2c                   ENDIF
423200030515      *
423300030515e   1c                   ENDIF
423400030515      *
423500030515      * Vedo se ha contrassegno
423600060505     c     KLDV          chain     FIAR9000
423700060505if  1c                   IF        %found(FIAR901L)
423800061030     c                   eval      DB0cas =  AR9cas
423900061030     c                   eval      DB0vca =  AR9vca
424000061030     c                   eval      DB0gca =  AR9gca
424100061030     c                   eval      DB0tic =  AR9tic
424200030515x   1c                   ELSE
424300030515     c                   clear                   DB0cas
424400030515     c                   clear                   DB0vca
424500030515     c                   clear                   DB0gca
424600030515     c                   clear                   DB0tic
424700030515     c                   clear                   DB0dti
424800030515     c                   clear                   DB0ucm
424900030515     c                   clear                   DB0ste
425000030515e   1c                   ENDIF
425100121018      *
425200131015      * Se P.A. con fattura immediata riporterò il Codice Fiscale
425300121018      *   (NON se destinatario estero)
425400121026if  1c                   if           ( %subst(DB0tb1:1:1) = 'A'
425500121018     c                             and (DB0nft  <> *zeros
425600121018     c                              or  DB0ift  <> *zeros)
425700121018     c                             and  DB0nzd  =  *blanks )
425800121018     c                   exsr      RepAR4_Q
425900121018     c                   movel     §TA4Qcfd      DB0cpi
426000121018e   1c                   endif
426100121018     c                   eval      wB0cfi = DB0cpi
426200061030      *
426300030515      * Vedo se si tratta di bolla figlia:
426400040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
426500070515if  1c                   if        $Mamma =  *on
426600030911     c                   exsr      BollaFiglia
426700030515e   1c                   endif
426800030515      *
426900030515     c                   ENDSR
427000030515      *
427100030515      *---------------------------------------------------------------*
427200050217      *?Decodifica LNA                                               ?*
427300030515      *---------------------------------------------------------------*
427400131203     c     sr_DecLNA     BEGSR
427500030515      *
427600030515if  1c                   if            DB0dla =  *blanks
427700030515     c                             and DB0ris <> 'M'
427800030515      *
427900040311     c                   eval      SAVlna = DB0lna
428000070312     c                   clear                   OG146arr
428100061016     c                   clear                   OG148arr
428200070312     c                   clear                   OG150arr
428300030515     c     DB0lna        chain     AZORG
428400030515      *
428500030515if  2c                   if        %found(AZORG01L)
428600030515     c                   eval      DB0fag = ORGfag
428700030515     c                   eval      DB0dla = ORGdes
428800030515     c                   eval      DB0inl = ORGind
428900030515     c                   eval      DB0cal = ORGcpf
429000030515     c                   eval      DB0lol = ORGloc
429100030515     c                   eval      DB0prl = ORGpro
429200030515     c                   eval      DB0tel = ORGtel
429300030515     c                   eval      DB0fax = ORGfax
429400070312     c                   movel     ORGde6        OG146arr
429500061016     c                   movel     ORGde8        OG148arr
429600070312     c                   movel     ORGdf0        OG150arr
429700030515e   2c                   endif
429800030515      *
429900030515e   1c                   endif
430000030515      *
430100030515     c                   ENDSR
430200030515      *
430300030515      *---------------------------------------------------------------*
430400050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
430500030515      *---------------------------------------------------------------*
430600030515     c     RepTASSAZ     BEGSR
430700030515      *
430800030515     c                   eval      DB0imv = AR6imv
430900030515     c                   eval      DB0cei = AR6cei
431000030515     c                   eval      DB0ift = AR6ift
431100101213      *
431200030911      * Esenzione IVA
431300030911if  1c                   if            DB0cei <> *blanks
431400030911     c                             and DB0dce =  *blanks
431500030911     c                   exsr      Tab_EI
431600030911e   1c                   endif
431700101213      *
431800101213      * Carico tabella VARIABILI ARRIVI
431900101213     c                   clear                   ds$2
432000101213     c                   eval      TBLcod = '$2'
432100101213     c                   eval      TBLkey = '1'
432200101213     c     K03TAB        chain     TABEL
432300101213if  1c                   if        %found(TABEL00F)
432400101213     c                   movel     TBLuni        ds$2
432500101213e   1c                   endif
432600030515      *
432700030515      * Controllo se ci sono varie esenti
432800030515      *
432900030515      * - varia 1
433000030515     c                   eval      Wsvx = AR6sv1
433100030515     c                   eval      Wvax = AR6va1
433200030515     c                   exsr      RepTASSAZ_b
433300030515      * - varia 2
433400030515     c                   eval      Wsvx = AR6sv2
433500030515     c                   eval      Wvax = AR6va2
433600030515     c                   exsr      RepTASSAZ_b
433700030515      * - varia 3
433800030515     c                   eval      Wsvx = AR6sv3
433900030515     c                   eval      Wvax = AR6va3
434000030515     c                   exsr      RepTASSAZ_b
434100030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
434200040607     c     KLDVtrc       setll     FIAR7000
434300040607     c     KLDVtrc       reade     FIAR7000
434400030515      *
434500030515do  2c                   Dow       Not %eof(FIAR701L)
434600030515      *
434700030515     c                   eval      Wsvx = AR7svn
434800030515     c                   eval      Wvax = AR7van
434900030515     c                   exsr      RepTASSAZ_b
435000030515      *
435100040607     c     KLDVtrc       reade     FIAR7000
435200030515e   2c                   Enddo
435300030515      *
435400030515     c                   ENDSR
435500030515      *
435600030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
435700050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
435800050217      *?- 2ª parte -                                                 ?*
435900030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
436000030515     c     RepTASSAZ_b   BEGSR
436100030515      *
436200030515sel 1c                   Select
436300030515      *
436400030515w   1c                   When      Wsvx       = *blanks
436500030515      *
436600030515w   1c                   When      Wsvx       = §$2iva
436700030515     c                   eval      DB0iva     = Wvax
436800030515      *
436900030515w   1c                   When      Wsvx       = §$2bol
437000030515     c                   add       Wvax          DB0ese
437100110117     c*//                z-add     Wvax          wBolloVirt
437200110117     c*//                eval      $BolloVirt = *on
437300030515      *
437400030515x   1c                   Other
437500101213      * Reperimento varie esenti IVA
437600101213     c                   movel     'CC'          TBLcod
437700101213     c                   movel(p)  Wsvx          TBLkey
437800101213     c     K03TAB        chain     TABEL
437900101213if  1c                   IF             %found(TABEL00F)
438000101213     c                             and  TBLflg = *blanks
438100101213     c                   movel     TBLuni        dsCC
438200101213if  2c                   if        §CCcei <> *blanks
438300101213     c                   add       Wvax          DB0ese
438400101213     c                   sub       Wvax          DB0imv
438500101213e   2c                   endif
438600101213e   1c                   ENDIF
438700030515      *
438800101213e   1c                   Endsl
438900030515      *
439000030515     c                   ENDSR
439100030515      *
439200030515      *---------------------------------------------------------------*
439300050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
439400030515      *---------------------------------------------------------------*
439500030515     c     TABxPAR       BEGSR
439600030515      *
439700100517     c                   movel(p)  WCdPar        w053a
439800030515     c                   movel     '7V'          TBLcod
439900030515     c                   movel(p)  KeyPAR        TBLkey
440000030515     c     K03TAB        chain     TABEL
440100030515      *
440200030515if  1c                   if        %found(TABEL00F)
440300030515     c                             and TBLflg = *blanks
440400100517     c                   eval      w053a = WCdPar + '-'
440500121108     c                                   + %trim(%subst(TBLuni:1:49))
440600030515x   1c                   else
440700030515     c                   clear                   TBLuni
440800030515e   1c                   endif
440900030515      *
441000030515     c                   ENDSR
441100030515      *
441200030515      *---------------------------------------------------------------*
441300050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
441400030515      *---------------------------------------------------------------*
441500030515     c     CtrCAs        BEGSR
441600030515      *
441700030515      * E' da convertire
441800030515if  1c                   if        DB0ucm = 'S'
441900030515     c                   eval      $CAeur = *on
442000030515e   1c                   endif
442100030515      *
442200030515      * Se in EURO è da convertire e da stampare in EURO
442300030515if  1c                   if        DB0vca = §GEDcn
442400030515     c                   eval      $CAeur = *on
442500030515     c                   endif
442600030515      *
442700030515      * Devo convertire il c/a da valuta a moneta di conto
442800030624if  1c                   if        DB0vca <> §GEDcn
442900030515     c                   clear                   YEURCODS
443000030515     c                   movel     DB0vca        YECdvi
443100030515     c                   z-add     DB0cas        YECimi
443200030515     c                   movel     §GEDcn        YECdvo
443300030515     c                   call      'YEURCO'
443400030515     c                   parm                    YEURCODS
443500030515if  2c                   if        YECesi = *blanks
443600030515     c                   z-add     YECimo        Wcas
443700030515e   2c                   endif
443800030515x   1c                   else
443900030515     c                   z-add     DB0cas        Wcas
444000030515e   1c                   endif
444100030515      *
444200030515     c                   ENDSR
444300030515      *
444400030515      *---------------------------------------------------------------*
444500050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
444600030515      *---------------------------------------------------------------*
444700030515     c     CtrDIV        BEGSR
444800030515      *
444900030515     c                   clear                   dsCV
445000030515      *
445100030515     c                   movel     'CV'          TBLcod
445200030515     c                   movel(p)  DB0div        TBLkey
445300030515     c     K03TAB        chain     TABEL
445400030515     c                   if        %found(TABEL00F)
445500030515     c                   movel     TBLuni        dsCV
445600030515     c                   movel     §CVfdc        DB0fdc
445700030515     c                   endif
445800030515      *
445900030515     c                   ENDSR
446000030515      *
446100030515      *---------------------------------------------------------------*
446200050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
446300030515      *---------------------------------------------------------------*
446400030911     c     BollaFiglia   BEGSR
446500030515      *
446600090619     c                   if        not %open(FNLBL01L)
446700101213     c                   open(e)   FNLBL01L
446800101213      /free
446900101213           if  %error;
447000101213             exsr  sr_ErrOpenFile;
447100101213           endif;
447200101213      /end-free
447300090619     c                   endif
447400090619      *
447500040607     c     KLDV          chain     FNLBL000
447600030515if  1c                   if        %found(FNLBL01L)
447700030515      *
447800030515     c     KLBL          chain     FNARB000
447900030515if  2c                   if        %found(FNARB01L)
448000030515      *
448100031015     c                   eval      wB0nrs = ARBnrs
448200030515     c                   eval      DB0fns = ARBfns
448300030515     c                   eval      DB0fls = ARBfls
448400030515     c                   eval      DB0ncd = ARBncd
448500030515     c                   eval      DB0nca = ARBnca
448600050114     c**!!!              eval      DB0xco = ARBxco
448700030515     c                   eval      DB0znc = ARBznc
448800030515      * Aggancio AR9
448900060505     c     KLBL          chain     FIAR9000
449000060505if  3c                   IF        %found(FIAR901L)
449100030515     c                   eval      DB0cas = AR9cas
449200030515     c                   eval      DB0vca = AR9vca
449300030515     c                   eval      DB0gca = AR9gca
449400030515     c                   eval      DB0tic = AR9tic
449500030515x   3c                   ELSE
449600030515     c                   clear                   DB0cas
449700030515     c                   clear                   DB0vca
449800030515     c                   clear                   DB0gca
449900030515     c                   clear                   DB0tic
450000030515     c                   clear                   DB0dti
450100030515     c                   clear                   DB0ucm
450200030515     c                   clear                   DB0ste
450300030515e   3c                   ENDIF
450400030515      *
450500030515e   2c                   endif
450600030515      *
450700030515e   1c                   endif
450800030515      *
450900030515     c                   ENDSR
451000030903      *
451100030515      *---------------------------------------------------------------*
451200050217      *?DEFINIZIONE KEY-LIST                                         ?*
451300030515      *---------------------------------------------------------------*
451400030515     c     DefKLIST      BEGSR
451500030515      *
451600090619      * - FNARB01L / FNART01L /
451700030515      *   FNLBL01L /
451800060505      *   FIAR501L/31C / FIAR901L
451900040607     c     KLDV          klist
452000030515     c                   kfld                    DB0aas
452100030515     c                   kfld                    DB0lnp
452200030515     c                   kfld                    DB0nrs
452300030515     c                   kfld                    DB0nsp
452400060505      * - FIAR401L / FIAR601L / FIAR701L
452500060505     c     KLDVtrc       klist
452600060505     c                   kfld                    DB0aas
452700060505     c                   kfld                    DB0lnp
452800060505     c                   kfld                    DB0nrs
452900060505     c                   kfld                    DB0nsp
453000060505     c                   kfld                    Wtrc
453100040607      * - FIAR501L/31C
453200030717     c     K05AR501      klist
453300030717     c                   kfld                    DB0aas
453400030717     c                   kfld                    DB0lnp
453500030717     c                   kfld                    DB0nrs
453600030717     c                   kfld                    DB0nsp
453700030717     c                   kfld                    AR5trd
453800090619      * - FNARB01L / FIAR901L di bolla MAMMA
453900070514     c     KLBL          klist
454000070514     c                   kfld                    LBLaap
454100070514     c                   kfld                    LBLlpp
454200070514     c                   kfld                    LBLnrp
454300070514     c                   kfld                    LBLnsp
454400081030      * - FNART01L di bolla MAMMA
454500081030     c     KLBLnsc       Klist
454600081030     c                   kfld                    LBLaap
454700081030     c                   kfld                    LBLlpp
454800081030     c                   kfld                    LBLnrp
454900081030     c                   kfld                    LBLnsp
455000081030     c                   Kfld                    Wnsc
455100081030     c     K05ARS01A     klist
455200081030     c                   kfld                    ARTfls
455300081030     c                   kfld                    ARTlna
455400081030     c                   kfld                    ARTnrs
455500081030     c                   kfld                    ARTnsc
455600081030     c                   kfld                    ARStrc
455700081030      * - FNART01L
455800081030     c     KLDVnsc       Klist
455900081030     c                   Kfld                    DB0aas
456000081030     c                   Kfld                    DB0lnp
456100081030     c                   Kfld                    DB0nrs
456200081030     c                   Kfld                    DB0nsp
456300081030     c                   Kfld                    Wnsc
456400100517      * - FNORM01L
456500100517     c     K04ORM01      klist
456600100517     c                   kfld                    ORMpoe
456700100517     c                   kfld                    ORMnsr
456800100517     c                   kfld                    ORMnor
456900100517     c                   kfld                    ORMnrv
457000030515      * - TABEL00F
457100030515     c     K03TAB        klist
457200040223     c                   kfld                    TBLkut
457300030515     c                   kfld                    TBLcod
457400030515     c                   kfld                    TBLkey
457500030515      *
457600030515     c                   ENDSR
457700080430
457800080430       //--------------------------------------------------------------
457900140303       //?Operazioni finali                                            ?
458000080430       //--------------------------------------------------------------
458100080430     c     sr_End        BEGSR
458200080430      *
458300080430     c                   clear                   TIBS02ds
458400080430     c                   eval      T02tla = 'C'
458500080430     c                   call      'TIBS02R'
458600080430     c                   parm                    KPJBA
458700080430     c                   parm                    TIBS02ds
458800080430     c                   reset                   FNLV80ds
458900080430     c                   call      'FNLV80R'
459000080430     c                   parm                    KPJBA
459100080430     c                   parm                    FNLV80ds
459200080430      /free
459300101222         if  §VPOrcm <= wDate and
459400101222             DB0tsp   = 'E'   and
459500101222            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
459600101222                                   DB0lnp <> 006   and
459700101222                                   DB0lnp <> 063));
459800101222           reset  TNSD99ds;
459900101222           tnsd99r ( TNSD99ds );
460000110117         endif;
460100080724
460200101222         // -?Chiusura del work-file?- - - - - - - - - - - - - - - - -*
460300090210
460400110202         if  %open(FIILV01L);
460500110202           close  FIILV01L;
460600090210         endif;
460700080430
460800101222         // -?Restituzione parametri?- - - - - - - - - - - - - - - - -*
460900080508
461000110221         if wCod_Log  <> '01';
461100130206           RootFolder = §ILVimgSca;
461200110221         endif;
461300110221
461400080520         if Risultato <> 'OK';
461500080718
461600080731           clear FileName;
461700080731           clear TemplateName;
461800080508
461900130213           for wx = 1 to c_Row;
462000130206             %occur(LdV) = wx;
462100130206             LdvPrg      = wx;
462200130206           endfor;
462300080508
462400080508         endif;
462500080430
462600131203
462700080430           exec sql   set result sets
462800080430                      for return to client
462900081103                      array :LdV   for :w_row rows;
463000080430
463100080430           select;
463200080430             when sqlCode = 100;
463300080430               RtnStatus  = sqlCode;
463400130924               //RtnOpCode  = 'SQLCODE   ';
463500130924               //RtnEsito   = -1;
463600080430             when sqlCode < *zero;
463700080430               RtnStatus  = sqlCode;
463800130924               //RtnOpCode  = 'SQLCODE   ';
463900130924               //RtnEsito   = -1;
464000080430           endsl;
464100080430
464200080430
464300080430         return;
464400080430
464500080430      /end-free
464600080430     c                   ENDSR
464700080430
464800080430      *---------------------------------------------------------------*
464900130227
465000140303       //--------------------------------------------------------------
465100140303       //?Spool di stampa (per e-mail).                                ?
465200140303       //--------------------------------------------------------------
465300130227
465400130227     oPrtEMAIL  e            EmlTxt            2
465500130227     o                       SDSpgm
465600130227     o                                        +   1 '-'
465700130227     o                                        +   1 'SPEDIZIONE GIÀ EL-
465800130227     o                                              ABORATA 100 VOLTE'
465900130227     o          e            EmlTxt      0
466000130227     o                       SDSpgm
466100130227     o                                        +   1 '-'
466200130227     o                                        +   1 'SPEDIZIONE GIÀ EL-
466300130227     o                                              ABORATA 100 VOLTE'
466400130227     o          e            EmlTxt      2
466500130227     o                                              'Spedizione     '
466600130227     o                                        +   3 'Distinta  '
466700130227     o          e            EmlTxt      1  1
466800130227     o                                              '------------------
466900130227     o                                              --'
467000130227     o                                        +   3 '----------'
467100130227     o          e            EmlDet      1
467200130227     o                       DB0aas        3
467300130227     o                       DB0lnp        3  +   1
467400130227     o                       DB0nrs        3  +   1
467500130227     o                       DB0nsp        3  +   1
467600130227     o                       P_Fgs2        3  +   3
467700130227     o                       P_Ndc2        3  +   1
467800130227     o          e            EmlEnd      2
467900130227     o                                              '***  Fine Lista  ***'
468000130227     o          e            EmlEnd      0
468100130227     o                                              '***  Fine Lista  ***'
468200080724
468300140303       //--------------------------------------------------------------
468400140303       //?Spool per immagine della LdV.                                ?
468500140303       //--------------------------------------------------------------
468600080430     oQSYSPRT   e            PRTtxt            3
468700080430     o                                              '   '
468800080430     o                       c_Char           +   0
468900080430     o          e            PRTrow      1
469000080430     o                       LDV              +   0
469100110201     o          e            PRTend      3
469200110201     o                       LastRow          +   0
469300080430
469400080718       //--------------------------------------------------------------
469500140303       //?Procedure name: getCurrentSystemName.                        ?
469600140303       //?Purpose:        Reperisce l'attributo di rete current System Name?
469700140303       //?Returns:        Current System Name                          ?
469800080718       //--------------------------------------------------------------
469900080718     p getCurrentSystemName...
470000080718     p                 b
470100080718
470200080718     d getCurrentSystemName...
470300080718     d                 pi             8a
470400080718
470500080718      /free
470600080718
470700080718       clear qUSEC;
470800080718       qUSBPRV = %size(qUSEC);
470900080718
471000080718       RetrieveNetworkAttributes( QWCRNETA_dataReturned
471100080718                                : %size(qWCRNETA_dataReturned)
471200080718                                : 1
471300080718                                : 'SYSNAME   '
471400080718                                : qUSEC
471500080718                                );
471600080718
471700080718       if  qUSEI <> *blank   or
471800080718           qWCRNETA_dataReturned.qWCRDR.qWCNBRAR = *zero;
471900080718         dump(A);
472000080718         clear qWCRNETA_networkAttributeInformationTable;
472100080718       else;
472200080718         qWCRNETA_networkAttributeInformationTable =
472300080718                                       qWCRNETA_dataReturned.qWCARRAY;
472400080718       endif;
472500080718
472600080718       return  qWCRNETA_networkAttributeInformationTable.qWCDATA;
472700080718
472800080718      /end-free
472900080718
473000080718     p getCurrentSystemName...
473100080718     p                 e
473200080718
473300110202       //--------------------------------------------------------------
473400110202       //?Schiere a tempo di compilazione.                             ?
473500110202       //--------------------------------------------------------------
473600030515
473700030515** > $GgS/$Gio * --------------------------------------------------------------*
4738000306241LUNEDI
4739000306242MARTEDI
4740000410263MERCOLEDI
4741000306244GIOVEDI
4742000306245VENERDI
4743000305156SABATO
4744000305157DOMENICA
474500030515M          MATT.
474600030515P          POM.
474700110202** -?$CoL?(Codice LOG)?/?$DeL?(Decodifica codice LOG)?
47480011020200Spedizione in distinta chiusa - da elaborare
47490011020201Spedizione elaborata
47500011032808Sped. non consegnata in dist. - immagine a SCARTO
47510011032509LdV da scannerizzare - immagine a SCARTO
47520011020321Parametri di input errati - immagine NON creata
47530011020822Distinta non trovata/annullata - immagine a SCARTO
47540011020823Sped./Dist. non trovata in FIILV - immag. a SCARTO
47550011020325Sped. non trovata in FNARB - immagine NON creata
47560013031351BOLLA RECUPERO - Immagine già generata
47570013031352BOLLA RECUPERO - Spedizione non trovata in FNARB
