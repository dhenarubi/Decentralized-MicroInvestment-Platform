import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const investor1 = accounts.get("wallet_1")!;
const investor2 = accounts.get("wallet_2")!;
const business = accounts.get("wallet_3")!;

describe("MicroInvestment", () => {
    it("successfully registers a business", () => {
        const registerCall = simnet.callPublicFn(
            "MicroInvestment",
            "register-business",
            [],
            business
        );
        expect(registerCall.result).toBeOk(Cl.bool(true));

        const businessInfo = simnet.callReadOnlyFn(
            "MicroInvestment",
            "get-business-info",
            [Cl.principal(business)],
            business
        );

        expect(businessInfo.result).toStrictEqual({
          type: 12, // Response type
          data: {
              'total-raised': {
                  type: 1,
                  value: 0n
              },
              'is-active': {
                  type: 3
              }
          }
        });

    });
    it("allows investment in a business", () => {
        const investAmount = 1000;
        const investCall = simnet.callPublicFn(
            "MicroInvestment",
            "invest",
            [Cl.uint(investAmount), Cl.principal(business)],
            investor1
        );
        expect(investCall.result).toBeOk(Cl.bool(true));

        const investmentInfo = simnet.callReadOnlyFn(
            "MicroInvestment",
            "get-investment",
            [Cl.principal(investor1)],
            investor1
        );

        expect(investmentInfo.result).toStrictEqual({
          type: 12,
          data: {
              amount: {
                  type: 1,
                  value: 1000n
              },
              'last-investment': {
                  type: 1,
                  value: 3n
              }
          }
        });

    });
});
