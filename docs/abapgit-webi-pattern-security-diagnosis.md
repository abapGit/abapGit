# Diagnóstico: Pattern / Security Level perdidos no import abapGit (Service Definition / WEBI)

> Última atualização: 2026-06-07
> Vale para os conectores v360 que usam Enterprise Services / proxies (SPRX): `sap_connector_fiscal_inbound`, `sap_connector_financial_services`, etc.

### Arquivos de referência (pasta `docs/`)

Os insumos brutos que sustentam este diagnóstico estão versionados ao lado deste markdown:

| Arquivo | Conteúdo | Usado em |
|---|---|---|
| [`Trace.trc`](./Trace.trc) | Trace SQL completo (ST05, export `;`-separado) do save manual de Pattern/Security/Operação no SPROXY, objeto `ZV360MMIIW_SI_ATUALIZAR_FORNEC`, em 2026-06-07 22:51. | Seção 7 |
| [`DOC_INTF_ZV360FIIIW_SI_FETCH_RECEIVABLE_ORIGINAL.WSDL`](./DOC_INTF_ZV360FIIIW_SI_FETCH_RECEIVABLE_ORIGINAL.WSDL) | WSDL do **sistema de origem** (correto, com bloco WS-Policy). | Seção 3 |
| [`DOC_INTF_ZV360FIIIW_SI_FETCH_RECEIVABLE_IMPORTADO A4H.WSDL`](./DOC_INTF_ZV360FIIIW_SI_FETCH_RECEIVABLE_IMPORTADO%20A4H.WSDL) | WSDL do **ambiente novo** após import abapGit (quebrado, sem WS-Policy). | Seção 3 |

## 1. Sintoma

Ao importar um Enterprise Service (proxy `SPRX`) via abapGit num ambiente novo, a aba **External View** do Service Provider vem com campos **em branco** no destino:

- **Pattern** (nível interface) — deveria vir `Stateless`
- **Security Level** — deveria vir `low`
- **Pattern do método/operação** (ex.: `METH FetchReceivables`) — deveria vir `normal`

A checagem de inconsistências do SPROXY acusa esses campos como ausentes.

Objetos de exemplo investigados:
- `INTF ZV360FIIIW_SI_FETCH_RECEIVABLE` (financial_services) — operação `FetchReceivables`
- `INTF ZV360MMIIW_SI_ALTERAR_PEDIDO`, `ZV360MMIIW_SI_ACEITE_FOLHA_REG` (fiscal_inbound)
- `INTF ZV360MMIIW_SI_ATUALIZAR_FORNEC` (fiscal_inbound) — operação `siAtualizaFornecedores_In`; usado no trace ST05 da seção 7 (save manual reproduzido e capturado em 2026-06-07).

## 2. Causa-raiz (confirmada)

Pattern e Security Level **NÃO pertencem ao objeto proxy `SPRX`**. Eles pertencem à **Service Definition / Web Service Definition**, que é um **objeto separado de tipo `WEBI`** (`R3TR WEBI`), nomeado `Z` + nome da Service Interface (ex.: `ZSI_FETCH_RECEIVABLES`).

Esses atributos são persistidos como **WS-Policy** na tabela **`WSSOAPPROP`** (e tabelas-irmãs `WSHEADER`, `VEPHEADER`, `VEPENDPOINT`, `VEPFUNCTION`, `VEPVISOAPEXT`), gravadas pelas classes `CL_WS_MD_WSD_PROPERTY` / `CL_WS_MD_*` / `CL_VIF_DB`.

### Como os 3 campos da tela viram dados no banco

Os **3 campos** que você escolhe na aba External View do SPROXY **não** são gravados como "3 valores". Cada campo é, internamente, um **profile** (um pacote pré-definido do SAP). Ao salvar, o SAP **expande** esse profile em **várias linhas** na tabela `WSSOAPPROP` — uma linha por "feature" do SOAP runtime. Resultado: **3 campos de tela → 9 linhas no banco**. É exatamente esse conjunto de linhas que se perde no import do abapGit (e o que precisa ser reposto na correção).

Como ler as tabelas abaixo:
- **Campo da tela** → **Profile**: o valor escolhido e o nome interno do profile correspondente (comprovado no trace ST05, seção 7 — o nome do profile é gravado na própria linha do `WSSOAPPROP`).
- **Feature / Propriedade / Valor**: cada linha gerada na `WSSOAPPROP`. `Feature` é o "namespace" SOAP; `Propriedade`+`Valor` é o par chave/valor efetivo.

Os prefixos de `Feature` são fixos (encurtados nas tabelas):
- Interface: `http://www.sap.com/webas/<630|640>/soap/features/…`
- Operação: `http://www.sap.com/NW05/soap/features/…` (+ uma `http://www.sap.com/esi/NW05/rif/`)

#### Campo 1 — Pattern (nível interface) = `Stateless` → profile `PRF_DT_IF_COM_STATELESS`

| Feature (encurtada) | Propriedade | Valor |
|---|---|---|
| `…/630/…/session/` | `enableSession` | `false` |
| `…/640/…/messageId/` | `enableMessageId` | `true` |

#### Campo 2 — Security Level = `low` → profile `PRF_DT_IF_SEC_LOW`

| Feature (encurtada) | Propriedade | Valor |
|---|---|---|
| `…/630/…/authentication/` | `AuthenticationLevel` | `Basic` |
| `…/630/…/transportguarantee/` | `Level` | `None` |

#### Campo 3 — Pattern do método/operação = `normal` → profile `PRF_DT_OP_COM_SYNC`

(no exemplo, operação `siAtualizaFornecedores_In`)

| Feature (encurtada) | Propriedade | Valor |
|---|---|---|
| `…/NW05/…/commit/` | `enableCommit` | `false` |
| `…/NW05/…/blocking/` | `enableBlocking` | `true` |
| `…/NW05/…/transaction/` | `required` | `no` |
| `…/NW05/…/wsrm/` | `enableWSRM` | `false` |
| `…/esi/NW05/rif/` | `mep` | `RequestResponse` |

**Resumo do mapeamento:** "Stateless" = `PRF_DT_IF_COM_STATELESS` (2 linhas); "low" = `PRF_DT_IF_SEC_LOW` (2 linhas); "normal" = `PRF_DT_OP_COM_SYNC` (5 linhas). As classes que escrevem essas linhas (`CL_WS_MD_WSD_PROPERTY` / `CL_WS_MD_*` / `CL_VIF_DB`) já constam no parágrafo inicial da seção.

Quando o `WEBI` não é importado, o proxy é recriado **sem** a Service Definition → essas 9 linhas não existem → o WSDL é gerado "pelado" → External View em branco.

## 3. Prova pela comparação de WSDL (objeto `SI_FETCH_RECEIVABLES`)

Arquivos comparados (ambos em `docs/`): [`…_ORIGINAL.WSDL`](./DOC_INTF_ZV360FIIIW_SI_FETCH_RECEIVABLE_ORIGINAL.WSDL) (origem, correto) × [`…_IMPORTADO A4H.WSDL`](./DOC_INTF_ZV360FIIIW_SI_FETCH_RECEIVABLE_IMPORTADO%20A4H.WSDL) (destino, quebrado).

A única diferença relevante é o **bloco inteiro de WS-Policy**, presente só na ORIGEM:
- namespaces `xmlns:wsp`, `xmlns:wsu` em `<wsdl:definitions>` (ORIGINAL linha 2; ausentes no IMPORTADO linha 2)
- `<wsp:UsingPolicy wsdl:required="true"/>` (ORIGINAL linha 6)
- `<wsp:Policy wsu:Id="IF_IF_SI_FETCH_RECEIVABLES">` → `enableSession=false` (**Stateless**) (ORIGINAL linhas 7‑11)
- `<wsp:Policy wsu:Id="OP_IF_OP_FetchReceivables">` → `enableBlocking=true` / `enableCommit=false` / `transaction=no` / `enableWSRM=false` (**Pattern da operação = normal**) (ORIGINAL linhas 12‑17)
- `<wsp:PolicyReference>` no `portType` (ORIGINAL linhas 210‑212) e na `operation` (linhas 214‑216); no IMPORTADO o `portType`/`operation` (linhas 196‑200) vêm **sem** qualquer `wsp:Policy`.

⚠️ **Atenção (nuance importante):** dos três campos, o WSDL só evidencia **Stateless** e **Pattern da operação = normal**. O **Security Level NÃO é renderizado em nenhum dos dois WSDLs** — `authentication`/`transportguarantee` não aparecem como `wsp:Policy`. Ou seja, comparar WSDLs **não** detecta a perda do Security Level; esse campo vive **exclusivamente** no `WSSOAPPROP` (ver seção 2 e o trace na seção 7). Isso reforça que o WSDL é uma renderização **parcial** da policy.

O XSD (types/messages) é idêntico nos dois. Há **um único** elemento a mais na origem (`miro_number` em `FetchReceivablesReq`, ORIGINAL linha 44, ausente no IMPORTADO que termina em `erp_posting_keys`, linha 31), mas isso é **um campo de dado do payload** (estrutura/message type, `R3TR TABL`) — **não tem nada a ver com o problema deste diagnóstico**. É só sinal de que o destino importou uma versão mais antiga da estrutura. **Fora de escopo aqui**; o foco é a ausência de Pattern/Security/Operação, que é o que faz o Enterprise Service ser carregado errado no cliente.

**Conclusão:** o WSDL/`SPROXWSDL` **não** é um cofre de dados independente — é só uma renderização (parcial) do que está no `WSSOAPPROP`/`WEBI`. A falta do bloco WS-Policy no WSDL = a falta do `WEBI` (Pattern/Security/Operação perdidos).

## 4. Hipóteses descartadas (onde o dado NÃO está)

- `SPROXHDR` (todos os campos, incl. `FURTHER_DATA`, `BO_DATA`)
- `SPROXDAT` (incl. `IFR_PATTRN`, `FURTHERDATA`)
- `SPROXSVARDAT`, `SPROXLPT`, `SPROXWSDL`
- Não é bug do serializer `zcl_abapgit_object_sprx`.
- A WSDL exibida no SPROXY é **regenerada** (em alguns casos com defaults), então não serve como fonte da verdade do External View.

## 5. abapGit suporta WEBI? SIM

- Handler: `src/objects/zcl_abapgit_object_webi.clas.abap` (existe desde 2018; abapGit local v1.133.0).
- A estrutura `ty_webi` serializa exatamente `pwssoapprop` (WSSOAPPROP) + `pwsheader` + todas as `pvep*`. Tem método `handle_soap`.
- `exists` via `cl_ws_md_vif_root=>check_existence_by_vif_name`; `deserialize` cria o VIF, faz `save( )` e `tadir_insert`.
- Em `zcl_abapgit_dependencies`: SPRX (`korrnum 135000`) e WEBI (`134000`) são ordenados juntos. `zcl_abapgit_file_deserialize` trata a dependência SPRX↔WEBI.

## 6. Mas o WEBI não aparece como opção de commit no abapGit

Fato observado: depois de preencher Pattern/Security manualmente e salvar, o `WEBI` passa a existir, mas o abapGit **não o oferece** para commit.

Verificação no sistema (TADIR) para `ZSI_FETCH_RECEIVABLES`:
- `PGMID=R3TR`, `OBJECT=WEBI`, `DEVCLASS=ZV360FS_S_FETCH_REC`, `GENFLAG=''`, `DELFLAG=''`, autor `IMORSE`.
- Ou seja: objeto **transportável, normal, não-gerado, no pacote rastreado pelo repo** (o proxy fica em `src/zv360_fs_soap/.../zv360fs_s_fetch_rec/`).

**CAUSA CONFIRMADA (não é ambiente — é decisão de design do abapGit):** o handler `zcl_abapgit_object_webi` **ignora de propósito** as Service Definitions marcadas como `auto_generated`.

- `zif_abapgit_object~exists` (linhas 448‑453): faz `SELECT auto_generated FROM vependpoint WHERE vepname = obj_name AND version = active`; se `auto_generated = 'X'` → `RETURN` com `rv_bool = abap_false`. Como o abapGit considera que o objeto "não existe", ele **nunca o lista no staging/commit**.
- `zif_abapgit_object~deserialize` (linhas 388‑393): mesmo teste; se `auto_generated = 'X'` → `RETURN` com comentário *"handled by SPRX"*.

Confirmação no dado (via MCP): `VEPENDPOINT` de `ZSI_FETCH_RECEIVABLES` tem `VERSION='A'`, `ENDPOINTTYPE='XIPR'`, **`AUTO_GENERATED='X'`**. As Service Definitions criadas ao salvar um proxy inbound são sempre `auto_generated='X'` → portanto sempre invisíveis ao abapGit.

A premissa do abapGit é: "a service definition auto-gerada é recriada automaticamente quando o SPRX é deserializado, então não preciso versioná-la". Isso quebra exatamente no nosso caso: o SPRX recria a service definition com **policy default** (sem as customizações), e Pattern/Security (que vivem só no `WSSOAPPROP` dessa WEBI auto-gerada) **se perdem** — sem nenhum caminho nativo no abapGit para commitá-las.

## 7. Anexo analítico — Trace ST05 do save manual

Fonte bruta: [`docs/Trace.trc`](./Trace.trc) — export ST05 completo (`;`-separado; cabeçalho `DATE;TIME;…;STATEMENT_WITH_VALUES;…;OPERATION;…;USER_NAME`). Capturado ao editar manualmente, no SPROXY, a External View do proxy `INTF ZV360MMIIW_SI_ATUALIZAR_FORNEC` (Service Interface `SI_ATUALIZAR_FORNECEDOR`, Service Definition `WEBI ZSI_ATUALIZAR_FORNECEDOR`) e gravar **Pattern = Stateless**, **Security Level = Low**, **Pattern do método/operação `siAtualizaFornecedores_In` = Normal**.

- Janela: `2026-06-07 22:51:51 → 22:51:54`, usuário `IMORSE`, conexão R/3 (HANA), transação `SEU_INT`.
- Para reproduzir o recorte do save, filtrar `Trace.trc` pelas tabelas `WSSOAPPROP`, `VEPHEADER`, `VEPENDPOINT`, `VEPFUNCTION`, `VEPVISOAPEXT`, `WSHEADER`, `TADIR`, `E071`, `SPROXWSDL` na janela `22:51:52.6 → 22:51:53.2`. O arquivo tem terminadores CRLF, então use `grep -a`: `grep -aE 'WSSOAPPROP|VEP|WSHEADER|SPROXWSDL' docs/Trace.trc`. As 9 features de policy aparecem como `WSSOAPPROP;INSERT` (uma por linha).
- O save é **uma única LUW** fechada por `COMMIT WORK ON CONNECTION 0` em `22:51:53.145` (a `22:51:54.330` é só o refresh da árvore do SEWB).

### 7.1 O que o SAP faz, em ordem (resumo do que o trace prova)

1. **Reserva/abre transporte.** `INTF` e `WEBI` são registrados na **mesma** request `A4HK900090`:
   - `TADIR INSERT … 'R3TR','INTF','ZV360MMIIW_SI_ATUALIZAR_FORNEC' … 'ZV360_MM'` (`22:51:52.699`)
   - `TADIR INSERT … 'R3TR','WEBI','ZSI_ATUALIZAR_FORNECEDOR' … 'ZV360_MM'` (`22:51:53.013`)
   - `TLOCK`/`E071 INSERT` para `INTF` (pos 002279) e `WEBI` (pos 002280). **Confirma: o WEBI é objeto transportável `R3TR WEBI` no pacote `ZV360_MM`.**
2. **Apaga a policy antiga** da Service Definition (versão inactive `I`) — limpa antes de regravar:
   - `WSHEADER DELETE WHERE WSNAME='ZSI_ATUALIZAR_FORNECEDOR' AND VERSION='I'` (`CL_VIF_DB`)
   - bloco de `WSSOAPPROP DELETE` (rc=100, nada a apagar ainda) para cada FEATURE de interface e de operação
   - `VEPENDPOINT/VEPFUNCTION/VEPVISOAPEXT/VEPHEADER DELETE` (versão `I`) — classes `CL_WS_MD_VIF_*`, `CL_WS_MD_SOAP_EXT_VIRTINFC`.
3. **Recria os metadados da VIF** (cabeçalho + endpoint + função + extensão SOAP):
   - `VEPHEADER INSERT` (`ZSI_ATUALIZAR_FORNECEDOR`,`I`,`SI_ATUALIZAR_FORNECEDOR`,…)
   - `VEPVISOAPEXT INSERT` (namespace `urn:sap-com:sprx:ep:cust:v360.com.br`, app `urn:sap-com:soap:application:esr:server:710`)
   - `VEPENDPOINT INSERT` (`VI`,`ZSI…`,`I`,0,`XIPR`, … aponta para `ZV360MMIIW_SI_ATUALIZAR_FORNEC`)
   - `VEPFUNCTION INSERT` (`ZSI…`,`I`,`SI_ATUALIZA_FORNECEDORES_IN`,`siAtualizaFornecedores_In`)
4. **Grava a policy nova** — estes `WSSOAPPROP INSERT` são a materialização exata dos três campos editados:

   | Momento | FEATURE | PROPNAME | Valor | Profile | Campo da tela |
   |---|---|---|---|---|---|
   | `…53.125` | `.../webas/640/.../messageId/` | `enableMessageId` | `true` | `PRF_DT_IF_COM_STATELESS` | Pattern = **Stateless** |
   | `…53.126` | `.../webas/630/.../session/` | `enableSession` | `false` | `PRF_DT_IF_COM_STATELESS` | Pattern = **Stateless** |
   | `…53.128` | `.../webas/630/.../authentication/` | `AuthenticationLevel` | `Basic` | `PRF_DT_IF_SEC_LOW` | Security = **Low** |
   | `…53.129` | `.../webas/630/.../transportguarantee/` | `Level` | `None` | `PRF_DT_IF_SEC_LOW` | Security = **Low** |
   | `…53.130` | `.../NW05/.../commit/` | `enableCommit` | `false` | `PRF_DT_OP_COM_SYNC` | Operação = **Normal** |
   | `…53.131` | `.../NW05/.../blocking/` | `enableBlocking` | `true` | `PRF_DT_OP_COM_SYNC` | Operação = **Normal** |
   | `…53.132` | `.../NW05/.../transaction/` | `required` | `no` | `PRF_DT_OP_COM_SYNC` | Operação = **Normal** |
   | `…53.133` | `.../NW05/.../wsrm/` | `enableWSRM` | `false` | `PRF_DT_OP_COM_SYNC` | Operação = **Normal** |
   | `…53.134` | `.../esi/NW05/rif/` | `mep` | `RequestResponse` | `PRF_DT_OP_COM_SYNC` | Operação = **Normal** |

   Todas via `CL_WS_MD_WSD_PROPERTY` (`REEXEC`, rc=0). As features de interface têm `FUNCREF=''`; as de operação têm `FUNCREF='siAtualizaFornecedores_In'`. Cada INSERT é precedido de um `DELETE`/`UPDATE` da mesma chave com `PROPNUM='0000'`→`'0001'` (padrão de "regravar a entrada efetiva").
5. **Fecha o cabeçalho** da WSD: `WSHEADER UPSERT` (`ZSI_ATUALIZAR_FORNECEDOR`, path `/sap/bc/srt/xip/sap/`, versão `I`).
6. **Texto/SOTR**: `SOTR_HEAD/SOTR_TEXT INSERT` com descrição *"Proxy Interface (generated)"* e `SOTR_USE INSERT` (`R3TR WEBI ZSI_ATUALIZAR_FORNECEDOR`).
7. **Invalida o cache de WSDL**: `SPROXCLASS DELETE` + `SPROXWSDL DELETE` (`CL_PROXY_WSDL_CACHE`). **Confirma a seção 3:** o WSDL/`SPROXWSDL` é cache regenerável, não fonte da verdade — a policy real vive em `WSSOAPPROP`.
8. **`COMMIT WORK`** (`22:51:53.145`) + log de dicionário (`DDLOG INSERT`).

### 7.2 Conclusões reforçadas pelo trace

- **Os três campos da External View são gravados como linhas `WSSOAPPROP` da `WEBI`, não no `SPRX`.** Nenhum `SPROXHDR/SPROXDAT` recebe esses valores no save — só `WSSOAPPROP`/`VEP*`/`WSHEADER` (+ `SPROXHDR UPSERT` apenas do cabeçalho do proxy, sem as features). Isto fecha a causa-raiz da seção 2 com evidência de escrita direta.
- **O profile carimbado em cada linha torna o mapeamento determinístico** para qualquer correção: para reproduzir "Stateless / Low / Normal" basta regravar as 9 linhas acima (versões `A` e `I`) com seus profiles.
- **O WEBI é, sim, transportável e versionado** (TADIR/E071/TLOCK em `A4HK900090`) — o que o abapGit não enxerga é só porque `VEPENDPOINT.AUTO_GENERATED='X'` (seção 6), não porque o objeto seja "não-transportável".
- **Plano de correção (seção 8) confirmado e parametrizável:** o patch/workaround precisa reaplicar exatamente este conjunto `WSSOAPPROP` após o SPRX recriar a VIF auto-gerada. As chaves e valores estão acima, prontos para um report de pós-import ou para o `deserialize` do handler.

## 8. Correção

### 8.0 Por que "só commitar o WEBI" não resolve (o problema tem 2 camadas)

1. **Visibilidade:** `zif_abapgit_object~exists` (linhas 448‑453) faz `SELECT auto_generated` e **retorna `false`** quando `='X'`. Logo o abapGit nunca lista a service definition no staging → **`serialize` nunca roda para ela** → **não existe arquivo** `*.webi.xml` com a policy.
2. **Persistência:** o serializer do SPRX (`zcl_abapgit_object_sprx`) usa `cl_proxy_db=>serialize`, que captura **só `sproxhdr`/`sproxdat`** — **não** a `WSSOAPPROP`. Então, hoje, as 9 linhas de policy (seção 2) **não estão em nenhum arquivo do repo**. E mesmo que estivessem, `deserialize` (linhas 388‑393) faz `RETURN` para `auto_generated`.

Ou seja: corrigir exige (a) **capturar** a policy num arquivo versionado e (b) **reaplicá-la** no destino depois que o SPRX recria a service definition com policy default.

### 8.1 Opção 1 — Patch no abapGit (recomendado; resolve de vez, sem ação manual no destino)

#### Onde fazer o hook (e por quê: o WEBI roda ANTES do SPRX)

Mapa do fluxo no abapGit (refs reais):
- **Ordenação:** `zcl_abapgit_file_deserialize=>prioritize_deser` (linhas 190‑206) faz o **SPRX *exigir* o WEBI** (`WHEN 'SPRX' … DELETE lt_requires WHERE obj_type <> 'WEBI'`), e `zcl_abapgit_dependencies` dá korrnum WEBI=`134000` < SPRX=`135000`. Resultado comprovado pelo teste `webi_before_sprx` (testclasses): **WEBI deserializa primeiro, SPRX depois.**
- **Por isso, reaplicar policy "depois do SPRX" NÃO cabe no `zcl_abapgit_object_webi`** — ele já rodou antes e, para `auto_generated`, faz `RETURN` (linhas 388‑393). Além disso o WEBI auto-gerado é invisível (`exists`=`false`, linhas 448‑453), então **nem existe arquivo dele**.
- **Conclusão: todo o patch fica em `zcl_abapgit_object_sprx`** — o objeto que de fato é versionado, roda por último e cuja deserialização (`db_save` + ativação) é o que recria a service def com policy default. É exatamente *depois* desse ponto que precisamos sobrepor a policy.

#### Mudanças concretas em `zcl_abapgit_object_sprx`

Guardar tudo atrás da condição "é proxy de Web Service": só agir quando `mv_object = 'INTF'` e o header for `gen_appl = 'WEBSERVICES'` (mesma checagem que `load_db` já faz na linha 168). Para os demais proxies, nada muda.

**1) `serialize` (hoje linhas 384‑434):** após os dois `io_xml->add` existentes (`PROXY_HEADER`/`PROXY_DATA`), acrescentar um **terceiro nó no mesmo `.xml`**:
- Resolver o nome da service definition auto-gerada ligada a este proxy. O vínculo está em `VEPENDPOINT` (no trace, o endpoint da WEBI `ZSI_ATUALIZAR_FORNECEDOR` aponta para o proxy `ZV360MMIIW_SI_ATUALIZAR_FORNEC`); resolver via `SELECT vepname FROM vependpoint WHERE … auto_generated = 'X'` ou `cl_ws_md_vif_root`. *(Sub-tarefa de implementação: confirmar a query exata do mapeamento proxy→vepname.)*
- Ler a policy com `WEBI_GET_OBJECT` (a **mesma FM** que `zcl_abapgit_object_webi~serialize` usa, linhas 519‑545) e reaproveitar a estrutura `ty_webi`. Guardar **só o subconjunto de policy**: `pwssoapprop` + `pwsheader` + `pvepvisoapext` (e, se necessário p/ a chave da operação, `pvepfunction`/`pvependpoint`).
- **Limpar campos voláteis** antes de gravar, espelhando `zcl_abapgit_object_webi~serialize` (linhas 563‑588): em `pwsheader` zerar `author/createdon/changedby/changedon/ctime/utime`; em `pvepheader` zerar idem + `text_id/utime/wsint_version`; em `pvependpoint` zerar `clustd`. (As linhas de `pwssoapprop` não têm timestamp — são estáveis.) Sem isso o diff fica sujo a cada commit.
- Serializar apenas a versão ativa (`A`); a ativação no destino propaga para `I`.
- `io_xml->add( iv_name = 'WEBI_POLICY' ig_data = ls_policy )`.

**2) `deserialize` (hoje linhas 285‑309):** **depois** de `save( … )` + `COMMIT WORK` + `check_sprx_tadir( )` (ponto em que a service def já foi recriada com policy default):
- `io_xml->read( iv_name = 'WEBI_POLICY' CHANGING cg_data = ls_policy )`. **Se vier vazio, não fazer nada** (compatível com repos antigos sem o nó — mesma checagem `IS INITIAL` que o próprio SPRX usa no `delta_handling`, linha 127).
- Reaplicar a policy na service def já existente. Duas vias (ver 8.1-b):
  - **Preferida (API):** `cl_ws_md_factory=>get_vif_root( )->get_virtual_interface( lv_vepname )`, `lock( )`, reaplicar os 3 profiles (`PRF_DT_IF_COM_STATELESS`, `PRF_DT_IF_SEC_LOW`, `PRF_DT_OP_COM_SYNC`) via os setters de SOAP extension, `save( )`, `unlock( )`, e marcar para ativação (`zcl_abapgit_objects_activation=>add_item`).
  - **Fallback (determinístico, "trace-faithful"):** gravar as linhas `WSSOAPPROP`/`WSHEADER` (versões `A` e `I`) com `cl_proxy_data`/MODIFY guardado e **regenerar/ativar** o proxy para reconstruir o cache `SPROXWSDL` (o trace mostra `SPROXWSDL DELETE` no save — sem reativar, o WSDL fica velho).
- **Idempotência:** se a policy efetiva já igual à do nó, o reapply é no-op (a API não muda nada; o fallback compara antes de gravar).

> ⚠️ **Ponto a confirmar na implementação:** a API exata de "reaplicar profile" no `if_ws_md_*`. Observação relevante: o `handle_soap` atual do WEBI (linhas 218‑239) só cria o `soap_extension_virtinfc` e seta o namespace — **não** grava as linhas de feature/propriedade do `WSSOAPPROP`. Ou seja, hoje nem o caminho não-auto_generated restaura as 9 features via API. Por isso o **fallback** (escrever `WSSOAPPROP` + regenerar) é o caminho mais garantido enquanto a API de profile não for confirmada; é o que o trace da seção 7 comprova funcionar.

#### 8.1-b Alternativa descartada — destravar o WEBI `auto_generated`

Trocar os `RETURN` de `exists` (448‑453) e `deserialize` (388‑393) do WEBI para versioná-lo mesmo auto-gerado. **Não recomendada:** exigiria **inverter a ordem** (hoje WEBI→SPRX) para reaplicar *depois* do SPRX, lidar com lock da VIF (`if_ws_md_lockable_object~lock`) e com o conflito de a service def ainda não existir quando o WEBI roda. Mais peças móveis e mais risco que concentrar tudo no SPRX (8.1 acima).

**Build:** `npm install && npm run build` (ou `npm run merge`) → gera `zabapgit.abap` standalone; instalar no sistema de teste e rodar o round-trip da seção 8.3.

### 8.2 Opção 2 — Workaround sem patch (rápido, para destravar já)

Report Z standalone, rodado **no destino após o import**, que para cada Service Interface afetada reaplica os 3 profiles na service definition `Z+<SI>` (versões `A` e `I`) e reativa o proxy. Valores de referência (seção 2): `enableSession=false`, `enableMessageId=true`, `AuthenticationLevel=Basic`, `Level=None`, e na operação `enableCommit=false`/`enableBlocking=true`/`required=no`/`enableWSRM=false`/`mep=RequestResponse`.
- Preferir a API `CL_WS_MD_*` (reaplicar profile) a INSERT direto em `WSSOAPPROP`; INSERT cru só como último recurso e sempre seguido de regeneração/ativação do proxy.
- Pode ser empacotado num transport para repetir em outros ambientes.

### 8.3 Como testar

**Round-trip do patch (opção 1) — o teste que prova a correção:**
1. **Origem (sistema com policy correta):** `serialize`/stage do proxy afetado. Conferir que o `.xml` do SPRX agora carrega o nó `WEBI_POLICY`: `grep -i "WEBI_POLICY\|WSSOAPPROP\|enableSession\|AuthenticationLevel" <arquivo .xml do SPRX no repo>` deve achar as entradas.
2. **Destino limpo:** `pull`/deserialize do mesmo commit.
3. **Verificar no destino (todas devem passar):**
   - SPROXY → aba External View do Service Provider mostra **Pattern=Stateless**, **Security Level=low**, **Pattern da operação=normal** (sem campos em branco).
   - A **checagem de inconsistências do SPROXY** não acusa mais os 3 campos.
   - SQL: `SELECT wsname version feature propname value FROM wssoapprop WHERE wsname = 'ZSI_<SI>'` → retorna as **9 linhas** descritas na seção 2, para **ambas** as versões (`A` e `I`).
   - WSDL regenerado pelo SPROXY contém de novo o bloco `<wsp:Policy>` (comparar com a origem — ver seção 3; lembrar que Security não aparece no WSDL, só via SQL).
4. **Idempotência:** re-`serialize` no destino → **diff vazio** (round-trip estável; sem campos voláteis vazando).
5. **Regressão:** repetir um pull num proxy **sem** customização de policy → não deve quebrar (a policy default continua válida) e não deve gerar diff espúrio.

**Teste do workaround (opção 2):** rodar o report no destino e aplicar as mesmas verificações do passo 3 acima. Bônus: exportar o WSDL do destino e comparar com [`…_ORIGINAL.WSDL`](./DOC_INTF_ZV360FIIIW_SI_FETCH_RECEIVABLE_ORIGINAL.WSDL) — o bloco WS-Policy deve passar a bater.

## 9. Itens em aberto / próximos passos

- Decidir entre patch (opção 1, todo dentro de `zcl_abapgit_object_sprx`) vs workaround (opção 2).
- Confirmar a **query exata** proxy→`vepname` da service def auto-gerada (via `VEPENDPOINT`/`cl_ws_md_vif_root`) usada no `serialize`.
- Confirmar a **API de reaplicar profile** (`if_ws_md_*`); enquanto não confirmada, usar o fallback "escrever `WSSOAPPROP` + regenerar/ativar" (8.1, comprovado pelo trace da seção 7).
- Implementar o reapply **após** `save`/`COMMIT WORK`/`check_sprx_tadir` no `deserialize` do SPRX, com guarda `WEBI_POLICY IS INITIAL` (compatível com repos antigos) e idempotência; limpar campos voláteis no serialize (espelhar `zcl_abapgit_object_webi~serialize` linhas 563‑588).
- Validar em pelo menos um proxy **com** policy custom e um **sem**, cobrindo o teste de regressão da seção 8.3.
- Confirmar comportamento em versão 702 (o handler WEBI tem ressalvas de campos inexistentes em 702 — ver comentários em `handle_endpoint`).
