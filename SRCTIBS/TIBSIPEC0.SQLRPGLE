000100150522     h decedit('0,') datedit(*dmy) option(*nodebugio)
000200150522     h DATFMT(*ISO)
000201150522
000300080930      // ----------------------------------------------------------------------
000400080930      //
000600080930      //
000900080930      // ----------------------------------------------------------------------
001500150519
002500080930     d §azute        e ds                  extname(azute00f)
002600080930     d                                     dtaara
002700080930     d §datiute      e ds                  extname(ddatiute)
003500150519     d tibsipec1i    e ds                  qualified inz(*EXTDFT)
003501150519     d tibsipec1o    e ds                  qualified inz(*EXTDFT)
003502150519     d tibsipec2i    e ds                  qualified inz(*EXTDFT)
003503150519
003505150519     d azpecds       e ds                  qualified extname(azpec00f)
003506150519     d ansogds       e ds                  qualified extname(ansog00f)
003507150521
003508150521     D BinArr          S              4B 0 Dim(12)
003509150522     D wrk_cdfisc      s             16    varying
003510150522     D wrk_data        s               D
003511150522     D wrk_tipopec     s              3
003512150604
003513150604      /COPY GAITRASRC/SRCCONST,TIBSIPEC0
003514150604      /COPY GAITRASRC/SRCPROTOPR,TIBSIPEC0
003516150519
003517150519     D Get_Indirizzo_PEC...
003518150519     D                 Pi
003519150519     D prmRqsOpCode...
003520150519     D                               10I 0 CONST
003521150519     D prmRpyOpCode...
003522150519     D                               10I 0
003523150519     D prmRqsFormat...
003524150519     D                               10A   CONST
003525150519     D prmRqsData...
003526150519     D                            32767A   OPTIONS(*VARSIZE) CONST
003527150519     D prmRqsDataSize...
003528150519     D                               10I 0 CONST
003529150519     D prmRpyFormat...
003530150519     D                               10A   CONST
003531150519     D prmRpyData...
003532150519     D                            32767A   OPTIONS(*VARSIZE)
003533150519     D prmRpyDataSize...
003534150519     D                               10I 0 CONST
004200150519
004700080930
004800080930      /free
004900080930
005201150519        select;
005202150519        when prmRqsOpCode = 100;
005203150521           prmRpyOpCode = *zeros;
005205150521           return;
005206150519
005207150519        when prmRqsOpCode = *zero;
005208150522
005209150522          exsr inzvar;
005210150522
005211150521          select;
005212150522
005213150519           when prmRqsFormat = 'TIBSIPEC1I';
005214150522
005215150522             tibsipec1i = %subst(prmRqsData:1:prmRqsDataSize);
005216150522           // controlli formato1
005217150522             exsr chk_formato1;
005218150522
005223150522           // reperisco azpec
005224150526           // wrk_data = tibsipec1i.data;
005225150522             wrk_tipopec = tibsipec1i.tipopec;
005226150522             wrk_cdfisc = tibsipec1i.cdfisc;
005227150522             exsr get_azpec;
005228150522
005229150522           // se tipo pec <> blanks faccio un tentativo senza tipo
005230150522           if tibsipec1i.tipopec <> *blanks
005231150522           and azpecds.indpec = *blanks;
005232150526           //  wrk_data = tibsipec1i.data;
005233150522             wrk_tipopec = *blanks;
005234150522             wrk_cdfisc = tibsipec1i.cdfisc;
005235150522             exsr get_azpec;
005236150522
005237150522           endif;
005238150522
005239150519           when prmRqsFormat = 'TIBSIPEC2I';
005240150522
005241150522             tibsipec2i = %subst(prmRqsData:1:prmRqsDataSize);
005242150522           // controlli formato2
005243150522             exsr chk_formato2;
005244150522
005250150522           // reperisco ansog
005251150522             exsr get_ansog;
005252150522           // reperisco azpec
005253150526           //  wrk_data = tibsipec2i.data;
005254150522             wrk_tipopec = tibsipec2i.tipopec;
005255150522             wrk_cdfisc = ansogds.sogcdfisc;
005256150522             exsr get_azpec;
005257150522           // se tipo pec <> blanks faccio un tentativo senza tipo
005258150522           if tibsipec2i.tipopec <> *blanks
005259150522           and azpecds.indpec = *blanks;
005260150526           //  wrk_data = tibsipec2i.data;
005261150522             wrk_tipopec = *blanks;
005262150522             wrk_cdfisc = ansogds.sogcdfisc;
005263150522             exsr get_azpec;
005264150522           endif;
005265150522
005266150522           other;
005267150604            // FORMATO NON VALIDO
005268150604           prmRpyOpCode = TIBSIPEC_FORMATO_INPUT_NON_VALIDO;
005269150522           return;
005270150522
005271150519           endsl;
005272150521
005273150521          exsr risposta;
005274150519
005275150519        endsl;
005276150604            // errore generico -1
005277150604           prmRpyOpCode = TIBSIPEC_ERRORE_DEFAULT;
005278150521           return;
005279150521
005280150522         begsr *pssr;
005281150522
005282150604            // errore da routine eccezione
005283150604           prmRpyOpCode = TIBSIPEC_ERRORE_PSSR;
005284150522           return;
005285150522
005286150522         endsr;
005287150519
005288150522         begsr *inzsr;
005289150522
005290150522           exec sql
005291150522            set option dynusrprf = *owner, closqlcsr = *endmod;
005292150522
005293150522         endsr;
005294150522
005295150519         begsr inzvar;
005296150519
005297150522           clear azpecds;
005299150521           clear ansogds;
005300150522           clear prmRpyData;
005301150522
005302150521          *inlr = *on;
005303150521
005304150519         endsr;
005305150519
005502150522         begsr get_azpec;
005503150522
005504150526            // imposto sempre data corrente
005505150526           wrk_data = %date(*date);
005506150526
005507150519           exec sql
005508150521             select *
005509150521               into :azpecds :binarr
005510150519               from azpec00f
005511150528             where azpec00f.codfisc = ucase(trim(:wrk_cdfisc))
005512150522               and azpec00f.tipopec = :wrk_tipopec
005513150522               and :wrk_data between azpec00f.dtinval and azpec00f.dtfival
005514150525               and (utentedel = '' or utentedel is null)
005515150519             ;
005516150603           if sqlcode < 0;
005517150521              prmRpyOpCode = sqlcode;
005518150521              return;
005519150603             // con sqlcode = 100 esco dal pgm solo se tipo = *blanks
005520150603             // cioè secondo tentativo reperimento
005521150603           elseif sqlcode = 100 and wrk_tipopec = *blanks ;
005522150603              prmRpyOpCode = sqlcode;
005523150603              return;
005524150603           endif;
005525150522
005526150519         endsr;
006000150519
006100150522         begsr get_ansog;
006105150521
006106150521           exec sql
006107150521             select ansog00f.sogcdfisc
006108150521               into :ansogds.sogcdfisc
006109150521               from anrco00f
006110150521               join ansog00f
006111150521                 on ansog00f.sogsogg = anrco00f.rcosogg
006112150521               where anrco00f.rcosnatura = :tibsipec2i.snatura
006113150521                 and anrco00f.rcoksc = :tibsipec2i.sottoconto
006114150522                 and anrco00f.rcosocieta = :tibsipec2i.societa
006115150521             ;
006116150521
006117150522         if sqlcode <> 0;
006118150521              prmRpyOpCode = sqlcode;
006119150521              return;
006120150521         endif;
006121150521
006132150519         endsr;
006133150519
006134150521         begsr risposta;
006135150521
006136150521            tibsipec1o.dtfival = azpecds.dtfival;
006137150521            tibsipec1o.dtinval = azpecds.dtinval;
006138150521            tibsipec1o.indpec = azpecds.indpec;
006139150521            tibsipec1o.tipopec = azpecds.tipopec;
006140150521            tibsipec1o.cdfisc = azpecds.codfisc;
006141150521
006142150521            prmRpyData = %subst(tibsipec1o:1:prmRpyDataSize);
006143150521
006144150521            prmRpyOpCode = 0;
006145150521            return;
006146150521
006148150519         endsr;
006149150519
006150150522         begsr chk_formato1;
006151150522
006157150522            if tibsipec1i.cdfisc = *blanks;
006158150604              prmRpyOpCode = TIBSIPEC_CODICE_FISCALE_OBBLIGATORIO;
006161150522              return;
006162150522            endif;
006163150522
006170150522           // data default
006171150526           // if tibsipec1i.data = *loval;
006173150526           //   tibsipec1i.data = %date(*date);
006174150526           // ENDIF;
006175150522
006176150522         endsr;
006177150522
006178150522         begsr chk_formato2;
006179150522
006180150522            if tibsipec2i.societa = *blanks;
006181150604              prmRpyOpCode = TIBSIPEC_SOCIETA_OBBLIGATORIA;
006182150522              return;
006183150522            endif;
006184150522
006185150522            if tibsipec2i.snatura <> 'C' and tibsipec2i.snatura <> 'F' ;
006186150604              prmRpyOpCode = TIBSIPEC_SOTTONATURA_NON_VALIDA;
006187150522              return;
006188150522            endif;
006189150522
006190150522            if tibsipec2i.sottoconto = *blanks;
006191150604              prmRpyOpCode = TIBSIPEC_SOTTOCONTO_OBBLIGATORIO;
006192150522              return;
006193150522            endif;
006194150522
006195150522           // controllo data
006196150526           // if tibsipec2i.data > %date(*date);
006197150526           //    prmRpyOpCode = -1;
006198150526           //    return;
006199150526           // ENDIF;
006200150522
006201150522           // data default
006202150526           // if tibsipec2i.data = *loval;
006203150526           //   tibsipec2i.data = %date(*date);
006204150526           // ENDIF;
006205150522
006206150522         endsr;
006207150522
