# com-cloudflare

Portable (`.cljc`) Cloudflare API v4 client -- REST + GraphQL Analytics +
Logpush job management. One tested auth/HTTP boundary, injectable transport,
for any kotoba-lang/gftdcojp project that needs to read Cloudflare zone,
Worker, Pages, or analytics state instead of re-deriving `curl`/HTTP-call
boilerplate ad hoc.

## Why this exists

`gftdcojp/cloud-itonami` built `cloud-itonami.analytics` (ADR-0010) as an
itonami-specific, one-off GraphQL Analytics client while investigating its
own portfolio's real traffic. That work also hand-rolled several ad hoc
`curl` calls (DNS records, Workers Custom Domains, Pages projects) to
answer one real question: *what actually serves this hostname?* --
`app.itonami.cloud` turned out to be bound to a different Worker's Custom
Domain, not visible from DNS or Pages alone (ADR-0010, 2026-07-04). Every
one of those was a real, useful capability that had no home to live in for
reuse by the next project that needs the same thing. This library is that
home.

## Design

```text
cloudflare.client      -- auth (CLOUDFLARE_API_TOKEN) + HTTP (injectable :http-fn) + GraphQL/REST envelopes
cloudflare.analytics   -- GraphQL Analytics: daily traffic trend, per-path/device/country/status breakdown
cloudflare.zones       -- zone list, DNS records (read)
cloudflare.workers     -- zone-scoped routes, account-scoped Custom Domains, worker scripts (read)
cloudflare.pages       -- Pages projects + their bound domains (read)
cloudflare.logpush     -- Logpush job management (list/create/delete) -- NOT log fetching/parsing, see below
```

Query/request construction and response parsing are pure `.cljc`. The
actual HTTP call is JVM-only by default (`java.net.http`) but every
function takes an injectable `:http-fn` (`{:url :method :headers :body} ->
{:status :body}`, the same convention `cloud-itonami.runtime`/
`cloud-itonami.mail` already use) -- every namespace here is tested with a
stub, never only against a live account.

## Usage

```clojure
(require '[cloudflare.analytics :as analytics])

;; CLOUDFLARE_API_TOKEN in the environment, or pass :token explicitly
(analytics/daily-report! {:zone-tag "..." :since (analytics/iso-date 7) :until (analytics/iso-date 0)})
;; => {:ok? true :days [...] :totals {:requests ... :page-views ... :uniques ... :bytes ...}}

(analytics/path-report! {:zone-tag "..." :since (analytics/iso-instant-hours-ago 23) :until (analytics/iso-instant-hours-ago 0)})
;; => {:ok? true :total ... :by-path {...} :by-device {...} :by-country {...} :by-status {...}}
;; host omitted = whole zone. A host-filtered call can legitimately return
;; zero rows if that host has no traffic while the zone does -- that's a
;; real finding, not a bug (see ADR-0010's app.itonami.cloud discovery).

(require '[cloudflare.workers :as workers])
(workers/custom-domains account-id)
;; => [{:hostname "app.itonami.cloud" :service "local-murakumo" ...} ...]
;; -- the answer to "what actually serves this hostname" when DNS/Pages don't have it.
```

## Logpush scope (honestly stated)

`cloudflare.logpush` manages Logpush *jobs* -- it does not fetch or parse
the log lines a job streams to its destination (R2/S3/GCS/Azure/Splunk/
...). Reading/parsing that stream is destination-specific and deliberately
out of scope here; build that in its own namespace once a real project has
picked a real destination, rather than speculatively ahead of one.

GraphQL Analytics only has aggregates (counts by dimension) -- it cannot
explain what a *specific* request's response, exception, or trace looked
like. `gftdcojp/cloud-itonami` hit exactly this wall investigating
unexplained edge-status-499s on one path
([net-kotobase#153](https://github.com/gftdcojp/net-kotobase/issues/153),
2026-07-04): aggregates confirmed the 499s were real but couldn't explain
why. A Logpush job on `http_requests` or `workers_trace_events`, wired to
a real destination, is the concrete next step for that kind of
investigation -- this namespace is the job-management half of getting
there.

## Test

```sh
clojure -M:test
```

Stubbed `:http-fn` throughout -- no `CLOUDFLARE_API_TOKEN` or live account
needed to run the suite.
