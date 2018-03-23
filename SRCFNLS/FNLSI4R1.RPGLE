000100990610     H DECEDIT('0,') DATEDIT(*DMY.)
000200170907     H* FNLSI4R1*----------------------------------------------------*
000300970410     H*                                                              *
000400151001     H*       - Ripristino bolle chiuse per merce mai affidata       *
000500170907     H*           Esecuzione aggiornamento                           *
000600970410     H*--------------------------------------------------------------*
000700170908     FFNBLP01L  uF   E           K DISK    commit(ii4fcmt) usropn
000701170908     f                                     infsr(*pssr)
000900170908     FFNBLT01L  UF   E           K DISK    commit(ii4fcmt) usropn
000901170908     f                                     infsr(*pssr)
000902170908     FFiart01L  UF   E           K DISK    commit(ii4fcmt) usropn
000903170908     f                                     infsr(*pssr)
002500970410     D*--------------------------------------------------------------*
002600970410     D*  SCHIERE
002700970410     D*--------------------------------------------------------------*
003000970414     D* ERRORI
003100170908     D ERR             S             78    DIM(02) CTDATA PERRCD(1)
003101170907
003300150204     d wdata           s              8  0
004701170907     d oggi            s              8  0
004702170908
004703170908     d comando         s            200a
004704170908     d cmdlen          s             15p 5
004705170908
004706170908     d qcmdexc         pr                  extpgm('QCMDEXC')
004707170908     d comando                      200a   options(*varsize) const
004708170908     d cmdlen                        15p 5 const
004709170908
004710170908     Dcmd              s             40    inz('STRCMTCTL LCKLVL(*CHG) CMTSCOPE-
004711170908     d                                     (*ACTGRP)')
004712170908     Dcmd1             s              9    inz('ENDCMTCTL')
004800970410     D*--------------------------------------------------------------*
004900970410     D* DS
005000970410     D*--------------------------------------------------------------*
005900080421      *
006000970410     D KPJBA         E DS
006001170907     D fnlsi4ds1     E DS
006200150204       // -?Parametri x Controllo profilo utenti?
006300150204     d TIBS34ds      e ds
006900150204       // -?Ds di riferimento al file esterno AZUTE00F?
007000150204     d AZUTEds       e ds                  extname(AZUTE00F)
007100150204       // -?Ds per dati organigramma?
007200150204     d dDATIUTE      e ds
009500151006     D fnlsu3ds        DS
009600151006     D  ilsu3flg               1      1
009700151006     D  ilsu3aas               2      5
009800151006     D  ilsu3lnp               6      8
009900151006     D  ilsu3nrs               9     10
010000151006     D  ilsu3nsp              11     17
010100151006     D  ilsu3DSP_B            18     25
010200151006     D  ilsu3DSP_S            26     33
010300151006     D  ilsu3nos              34     34
010400151006     d
010500151006     d  ilsu3tcr              35     35
010600151006     d  ilsu3dcr              36     43
010700151006     d  ilsu3hcr              44     47
010800151006     D  olsu3err             100    101
010900151006     D  olsu3boagg           102    110
011000151006     D  olsu3bonagg          111    116
011100151006     D  olsu3msg             117    193
011200151006     D  olsu3sosp            194    194
011201170419     D  Ilsu3NOAGG           201    201
011202180110     D  Ilsu3Fcmt            202    202
011203180110     D  Ilsu3Flo             203    212
012900150211
013000970410     C*--------------------------------------------------------------*
013100970410     C*  C I C L O     P R I N C I P A L E
013200970410     C*--------------------------------------------------------------*
013201170907     C     *ENTRY        PLIST
013202170907     C                   PARM                    KPJBA
013203170907     C                   MOVEL     KPJBU         fnlsi4ds1
013204170907     c*  pulizia campi di output
013205170907     c                   clear                   oi4err
013206170907     c                   clear                   oi4msg
013207170908      * TIPO LANCIO, Ii5TLA "C" -           CHIUSO CON LR
013208170908      * TIPO LANCIO, Ii5TLA "L" - ELABORO E CHIUDO CON LR
013209170908      * TIPO LANCIO, Ii5TLA " " - ELABORO E CHIUDO IN RETRN
013210170907
013211170907      /free
013212170907       If ii4tla<>'C';
013213170908           if not %open (fnblp01l);
013214170908              if Ii4STRCMT='1';
013215170908                 comando=cmd;
013216170908                 callp qcmdexc(comando : %size(comando));
013217170908              endif;
013218170908              exsr sr_openf;
013219170908           endif;
013220170907           exsr sr_elabora;
013221170907       endif;
013222170907
013223170907       kpjbu=fnlsi4ds1;
013224170907
013225170907       if ii4tla=' ';
013226170907           *inrt=*on;
013227170907       else;
013238170907           *inlr=*on;
013239170908           if %open(fnblp01l);
013240170908              exsr sr_closef;
013241170908              if Ii4STRCMT='1';
013242170908                 comando=cmd1;
013243170908                 callp qcmdexc(comando : %size(comando));
013244170908              endif;
013245170908           endif;
013246170907       endif;
013300150217
052800150204     C*--------------------------------------------------------------*
052900151001     C* Ripristino spedizione
053000150204     C*--------------------------------------------------------------*
053100170907     C     sr_elabora    BEGSR
053200150219     C     KBLT          CHAIN(e)  FNBLP01L
053300150219     c                   if        %error
053400170907     c                   eval      oi4msg=err(1)
053401170907     c                   eval      oi4err='E'
053600150219     c                   leavesr
053700150219     c                   endif
053800170907     C     KBLT          CHAIN     FNBLT01L                           32
053900170907     C     *in32         DOWEQ     '0'
054000151001     C                   clear                   BLTDFV
054100151001     C                   clear                   BLTNFV
054200151001     C                   clear                   BLTNPP
054300151001     C                   clear                   BLTDCM
054400151001     C                   clear                   BLTDTR
054500151001     C                   clear                   BLTFTR
054600151001     C                   clear                   BLTFL1
054700150204     C                   UPDATE    FNBLT000
054800150204     C* Lettura successiva
054900150204     C     KBLT          READE     FNBLT01L                               32
055000150204     C                   ENDdo
055100150204
055101170224     C     KBLT          CHAIN     Fiart01L                           32
055102170224     C     *IN32         DOWEQ     '0'
055103170224     C                   clear                   artDFV
055104170224     C                   clear                   artNFV
055105170224     C                   clear                   artNPP
055106170224     C                   clear                   artDCM
055107170224     C                   clear                   artdtc
055108170224     C                   clear                   artcca
055110170224     C                   UPDATE    Fiart000
055111170224     C* Lettura successiva
055112170224     C     KBLT          READE     Fiart01L                               32
055113170224     C                   ENDdo
055114170224
055200150204     C                   Z-ADD     0             BLPDCE
055300150204     C                   Z-ADD     0             BLPNPI
055400150204     C                   Z-ADD     0             BLPNCI
055500150204     C                   Z-ADD     0             BLPNRC
055600151001     C                   clear                   BLPDBR
055700151001     C                   clear                   BLPDPC
055800151001     C                   clear                   BLPDUC
055900151001     C                   clear                   BLPDDC
056000151001     C                   clear                   BLPDCM
056100150204     C                   TIME                    WTIME             6 0
056200151001     C                   clear                   BLPHMC
056300151001     C                   clear                   BLPNFV
056400151001     C                   clear                   BLPNDC
056500151001     C                   clear                   BLPDT1
056600151001     C                   clear                   BLPDT2
056700151001     C                   clear                   BLPFT1
056800151001     C                   clear                   BLPFT2
056900150204     C* Per idd metto codice consegna anomala sulla bolla
057000151001     C                   clear                   BLPCCA
057100150204     C* devo inviare date consegna cliente
057200150303     C                   clear                   BLPDT3
057300150303     C                   clear                   BLPFT3
057400150204     C* Update bolla
057500150204     C                   UPDATE    FNBLP000
057600151006     c* richiamo pgm fnlsu3r per aggiornamento data spedizione e data ritiro
057700151006     c                   clear                   fnlsu3ds
057800151006     c                   eval      ilsu3aas=%editc(blpaas:'X')
057900151006     c                   eval      ilsu3lnp=%editc(blplnp:'X')
058000151006     c                   eval      ilsu3nrs=%editc(blpnrs:'X')
058100151006     c                   eval      ilsu3nsp=%editc(blpnsp:'X')
058200151006     c                   eval      wdata=(blpaas*10000)+blpmgs
058300151006     c                   eval      ilsu3dsp_B=%editc(wdata:'X')
058400151006     c                   eval      ilsu3dsp_S=%editc(oggi:'X')
058500151006     c                   eval      ilsu3nos='S'
058600151006     c                   eval      ilsu3flg='R'
058601180110     c                   eval      ilsu3fcmt='1'
058700151006     c                   movel     fnlsu3ds      kpjbu
058800151006     c                   call      'FNLSU3R'
058900151006     c                   parm                    kpjbA
059000151006     c                   movel     kpjbu         fnlsu3ds
059001170908
059003171211                  if ii4cmtrlbk='1';
059004171211                     if olsu3err=*blanks;
059005170908                        commit;
059006171211                     else;
059007171211                        rolbk(e);
059008171211                     endif;
059009170908                  endif;
059100150204     c                   endsr
059200970410     C*--------------------------------------------------------------*
059300970410     C* *INZSR: Operazioni iniziali
059400970410     C*--------------------------------------------------------------*
059500970410     C     *INZSR        BEGSR
059900150204
060000150204     c     *dtaara       define    §azute        AZUTEds
060100150204     c     *dtaara       define    §datiute      dDATIUTE
060200150204     c                   in(E)     *dtaara
060300150204     c                   if        %ERROR or RSUT = *blanks
060400150204     c                   clear                   Tibs34Ds
060500150204     c                   call      'TIBS34R'
060600150204     c                   parm                    Tibs34Ds
060700150204     c                   in        *dtaara
060800150204     c                   endif
060900150204      *
061000150204     c                   z-add     1             CodUt             1 0
061100030924      *
061200030924      *
061300970410      *  Definizione chiavi
061900970410     C     KBLT          KLIST
062000170907     C                   KFLD                    ii4aas
062100170907     C                   KFLD                    ii4lnp
062200170907     C                   KFLD                    ii4nrs
062300170907     C                   KFLD                    ii4nsp
064400970410      *
065101170907       // Reperisco la data corrente
065102170907          OGGI=%dec(%date());
067000020912      *
067100970410     C*
067200970410     C                   ENDSR
067201170908       //-----------------------------------------------------------------
067202170908       // Open dei file
067203170908       //-----------------------------------------------------------------
067204170908       begsr sr_openf;
067205170908              open fnblp01l;
067206170908              open Fnblt01l;
067207170908              open FIARt01l;
067231170908       endsr;
067232170908       //-----------------------------------------------------------------
067233170908       // Close dei file
067234170908       //-----------------------------------------------------------------
067235170908       begsr sr_closef;
067236170908              close fnblp01l;
067237170908              close Fnblt01l;
067238170908              close FIARt01L;
067258170908       endsr;
067300080930      *------------------------------------------------------------------------*
067400080930      * Routine di controllo x eventuali Errori
067500080930      *------------------------------------------------------------------------*
067600080930     C     *PSSR         BEGSR
067601170908       oi4err='3';
067602170908       oi4msg=err(2);
067603170908       if ii4cmtrlbk='1';
067604170908          rolbk(e);
067605170908       endif;
067606170908       kpjbu=fnlsi4ds1;
067607170908       exsr sr_closef;
067608170908       if Ii4STRCMT='1';
067609170908          comando=cmd1;
067610170908          callp(e) qcmdexc(comando : %size(comando));
067611170908       endif;
067612170908       *inlr=*on;
067613170908       return;
067700080930      *
067800080930     C                   ENDSR     '*CANCL'
067900080930     C*------------------------------------------------------------------------*
068000971008**
069100170907Richiesta non elaborabile: bolla allocata da altro utente. Attendi e riprova  01
069200170908Riscontrati errori: Aggiornamento non eseguito                                02
