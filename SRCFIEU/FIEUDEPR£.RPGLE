000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200000000     H*--------------------------------------------------------------*
000300170921      *  DRIVER per controllo DEPOT VERSIONE CAPPARIO ANAGR./LEGAME
000400170921     H*--------------------------------------------------------------*
000500170921      **
000600170921      **  Ricevuti : 1) data 0 = UDATE oppure data >0 x reperire la VERSIONE
000700170921      **             2) Depot di Riferimento a 4  (se si ha il 4 solo questo)
000800170921      **             3) Depot di Riferimento a 7  (se si ha il 7 solo questo)
000900170921      **
001000170921      **  Ritorno  : 4) Flag Versione -> 0 = "OK" / 1 = "Non trovata Versione"
001100170921      **             5) Flag Cappario -> 0 = "OK" / 1 = "Non trovato Depot su Cappario"
001200170921      **             6) Flag Legame   -> 0 = "OK" / 1 = "Non trovato Depot su Legame"
001300170921      **             7) imposta il record ANAGRAFICO del cappario DPCDP10F
001400170921      **             8) imposta il record LEGAME Depot/cliente    DPLEG10F
001500170921      **
001600170921     H*--------------------------------------------------------------*
001700170921      *-- Depot a 7
001800170921     FDPcdp11i  iF   E           K DISK    usropn
001900170921     F                                     rename(DPCDP100:DPCDP11)
002000170921     FDPLEG11L  iF   E           K DISK    usropn
002100170921     F                                     rename(DPLEG000:DPLEG11)
002200170921     H*--------------------------------------------------------------*
002300170921      *-- Depot a 4
002400170921     FDPcdp12i  iF   E           K DISK    usropn
002500170921     F                                     rename(DPCDP100:DPCDP12)
002600170921     FDPLEG14L  iF   E           K DISK    usropn
002700170921     F                                     rename(DPLEG000:DPLEG14)
002800170921      *--------------------------------------------------------------*
002900170921     D WLBDA8          DS                  INZ
003000170921     D  G08DAT                 1      8  0
003100170921     D  G08INV                 9     16  0
003200170921     D  G08ERR                17     17
003300170921     D  G08TGI                18     22  0
003400170921      *--------------------------------------------------------------*
003500170921     dO_RecCAPPARIO  E ds                  extname(DPCDP10F)
003600170922     d                                     Prefix(DPD)
003700170921     dO_RecLegame    E ds                  extname(DPLEG10F)
003800170922     d                                     Prefix(BRT)
003900170921     D*------------------------------------------------------------------------*
004000170921     d FIEUdepDS     E ds
004100170921     D*------------------------------------------------------------------------*
004200170921     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
004300170921     C*------------------------------------------------------------------------*
004400170922      *    VERIFICA VERSIONE / ANAGRAFICA DPD / ANAGRAFICA LAGAMI CLIENTI_DEPOT
004500120531     C*------------------------------------------------------------------------*
004600170921     c     *Entry        plist
004700170921     c                   parm                    fieuDEPDS
004800170921     c                   parm                    O_RecCappario
004900170921     c                   parm                    O_RecLegame
005000170921      **
005100170921     c     Kcdp4         klist
005200170921     c                   kfld                    CDPATB
005300170921     c                   kfld                    CDPVER
005400170921     c                   kfld                    CDPDSTR
005500170921      **
005600170921     c     Kcdp7         klist
005700170921     c                   kfld                    CDPATB
005800170921     c                   kfld                    CDPVER
005900170921     c                   kfld                    CDPDPTN
006000170921      **
006100170921      **  se per errore è stato mandato sul Depot di 7 il Depot di 4
006200170921     c                   if        I_Depot4 =0 and I_Depot7 <= 9999
006300170921     c                   z-add     I_Depot7      I_Depot4
006400170921     c                   z-add     0             I_Depot7
006500170921     c                   end
006600170921      *
006700170921     c                   clear                   O_FlgVers
006800170921     c                   clear                   O_FlgCapp
006900170921     c                   clear                   O_FlgLega
007000170915      **
007100170921      ** il Depot ricevuto è a 4
007200170921     c                   if        I_Depot4 >0 and I_Depot7 =0
007300170921     c                   open      DPcdp12i
007400170921     c                   open      DPLEG14L
007500170921     c                   else
007600170921      ** il Depot ricevuto ha la B.U. quindi è a 7
007700170921     c                   open      DPcdp11i
007800170921     c                   open      DPLEG11L
007900170921     c                   end
008000170921      **
008100170921     C* CALCOLA LA DATA CORRENTE
008200170921     C                   time                    wn14             14 0
008300170921     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
008400170921     C                   movel     wn14          oracor            6 0          *ORA
008500170921     C*
008600170921     C                   z-add     wn8           g08dat
008700170921     C                   z-add     *zeros        g08inv
008800170921     C                   movel     '0'           g08err
008900170921     C                   call      'XSRDA8'
009000170921     C                   parm                    wlbda8
009100170921     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
009200170921      *
009300170921      *   imposta la data per reperire la versione
009400170921    1c                   z-add     I_DATAver     iSID1dri
009500170921    1c                   if        iSID1dri = 0
009600170921     C                   z-add     datCOR        iSID1dri
009700170922     C                   z-add     datCOR        I_DATAver
009800170921     c                   end
009900170921      * recupero  versione di cappario
010000170921     C                   clear                   ISID1DRI
010100170921     C                   clear                   OSID1VER
010200170921     C                   clear                   OSID1VERD
010300170921     C                   clear                   OSID1DDE
010400170921     C                   clear                   OSID1DSC
010500170921     C                   clear                   OSID1BCID
010600170921     c                   call      'TISID1R'
010700170921     C                   PARM                    ISID1DRI
010800170921     C                   PARM                    OSID1VER
010900170921     C                   PARM                    OSID1VERD
011000170921     C                   PARM                    OSID1DDE
011100170921     C                   PARM                    OSID1DSC
011200170921     C                   PARM                    OSID1BCID
011300170921      * se NON TROVATA VERSIONE CAPPARIO
011400170922     c                   move      '0'           O_FLGVERS
011500170921     c                   if        osid1ver <= 0
011600170921     c                   move      '1'           O_FLGVERS
011700170921     c                   RETURN
011800170921     c                   end
011900170921      ** ------
012000170921      **  Controlla   presenza in ANAGRAFICA CAPPARIO DPD
012100170921     c                   move      *blank        CDPATB
012200170921     c                   z-add     oSID1VER      CDPVER
012300170922     c                   move      '0'           O_FlgCapp
012400170921      ****   Depot (4)
012500170921     c                   if        I_Depot4 >0 and I_Depot7 =0
012600170921     c                   movel     I_Depot4      CDPDSTR
012700170921     c     Kcdp4         chain     dpcdp12i
012800170921      * non esiste ANAGRAFICA su CAPPARIO DPD
012900170921     c                   if        Not %Found(dpcdp12i)
013000170921     c                   move      '1'           O_FlgCapp
013100170921     c                   RETURN
013200170922     c                   end
013300170921      ****
013400170921     c                   else
013500170921      ****   Depot (7)
013600170921     c                   movel     I_Depot7      CDPDPTN
013700170921     c     Kcdp7         chain     dpcdp11i
013800170921      * non esiste ANAGRAFICA su CAPPARIO DPD
013900170921     c                   if        Not %Found(dpcdp11i)
014000170921     c                   move      '1'           O_FlgCapp
014100170921     c                   RETURN
014200170921     c                   end
014300170921      ****
014400170921     c                   end
014500170921      ** ------
014600170921      **  Controlla presenza del DEPOT fra i Legami
014700170921      ** ------
014800170921      ****   Depot (4)
014900170921     c                   if        I_Depot4 >0 and I_Depot7 =0
015000170921      ****
015100170921     c                   movel     I_Depot4      legDPC
015200170921     c                   move      '1'           O_FlgLega
015300170921     c     legDPC        setll     dpleg14l
015400170921     c     legDPC        reade     dpleg14l
015500170921      * non esiste ANAGRAFICA su CAPPARIO DPD
015600170921     c                   DOw       not %EoF(dpleg14l)
015700170921     c                   if        legDDE <= iSID1dri and
015800170921     c                             legDSC >= iSID1dri
015900170922     c                   move      '0'           O_FlgLega
016000170922     c                   leave
016100170921     c                   end
016200170921     c     legDPC        reade     dpleg14l
016300170921     c                   endDO
016400170921      ****
016500170921     c                   else
016600170921      ****   Depot (7)
016700170921     c                   movel     I_Depot7      legDep7
016800170921     c                   move      '1'           O_FlgLega
016900170921     c     legDEP7       setll     dpleg11l
017000170921     c     legDEP7       reade     dpleg11l
017100170921      * non esiste ANAGRAFICA su CAPPARIO DPD
017200170921     c                   DOw       not %EoF(dpleg11l)
017300170921     c                   if        legDDE <= iSID1dri and
017400170921     c                             legDSC >= iSID1dri
017500170921     c                   move      '0'           O_FlgLega
017600170922     c                   leave
017700170921     c                   end
017800170921     c     legDEP7       reade     dpleg11l
017900170921     c                   endDO
018000170921      ****
018100170921     c                   end
018200170922      *
018300170922      ****----------------------------------------
018400170922      **   reimposta i campi DEPOT
018500170922     c                   movel     CDPDSTR       I_Depot4
018600170922     c                   movel     CDPDPTN       I_Depot7
018700170922      ****----------------------------------------
018800170922      **
018900170922      *  Controlla se richiamato solo con la DS
019000170922     c                   if        %Parms > 1
019100170922      *
019200170922     c                   clear                   O_RecCappario
019300170922     c                   movel     CDPATB        DPDCDPATB
019400170922     c                   z-add     CDPVER        DPDCDPVER
019500170922     c                   movel     CDPDPTN       DPDCDPDPTN
019600170922     c                   movel     CDPDSTR       DPDCDPDSTR
019700170922     c                   movel     CDPIATA       DPDCDPIATA
019800170922     c                   movel     CDPGRPID      DPDCDPGRPID
019900170922     c                   movel     CDPNAME1      DPDCDPNAME1
020000170922     c                   movel     CDPNAME2      DPDCDPNAME2
020100170922     c                   movel     CDPSTRET      DPDCDPSTRET
020200170922     c                   movel     CDPSTNBR      DPDCDPSTNBR
020300170922     c                   movel     CDPADDR1      DPDCDPADDR1
020400170922     c                   movel     CDPADDR2      DPDCDPADDR2
020500170922     c                   movel     CDPFLOOR      DPDCDPFLOOR
020600170922     c                   movel     CDPBUILD      DPDCDPBUILD
020700170922     c                   movel     CDPISO2       DPDCDPISO2
020800170922     c                   movel     CDPSTATE      DPDCDPSTATE
020900170922     c                   movel     CDPPTC        DPDCDPPTC
021000170922     c                   movel     CDPCITY       DPDCDPCITY
021100170922     c                   movel     CDPTEL        DPDCDPTEL
021200170922     c                   movel     CDPFAX        DPDCDPFAX
021300170922     c                   movel     CDPEMAIL      DPDCDPEMAIL
021400170922     c                   movel     CDPWEB        DPDCDPWEB
021500170922     c                   z-add     CDPBUCN       DPDCDPBUCN
021600170922     c                   movel     CDPGEOX       DPDCDPGEOX
021700170922     c                   movel     CDPGEOY       DPDCDPGEOY
021800170922     c                   movel     CDPDUMY       DPDCDPDUMY
021900170922      *
022000170922     c                   clear                   O_RecLegame
022100170922     c                   movel     LEGDPC        BRTLEGDPC
022200170922     c                   movel     LEGNAR        BRTLEGNAR
022300170922     c                   z-add     LEGKSC        BRTLEGKSC
022400170922     c                   z-add     LEGDDE        BRTLEGDDE
022500170922     c                   z-add     LEGDSC        BRTLEGDSC
022600170922     c                   z-add     LEGDTV        BRTLEGDTV
022700170922     c                   z-add     LEGORV        BRTLEGORV
022800170922     c                   movel     LEGPRU        BRTLEGPRU
022900170922     c                   movel     LEGORMA       BRTLEGORMA
023000170922     c                   movel     LEGATB        BRTLEGATB
023100170922     c                   movel     LEGFLO        BRTLEGFLO
023200170922     c                   movel     LEGDEP7       BRTLEGDEP7
023300170922     c                   z-add     LEGBUN        BRTLEGBUN
023400170922      *
023500170922     c                   end
023600170922      ****
023700120531     C                   SETON                                        LR
023800081110     c                   return
023900090424     C*---------------------------------------------------------------*
