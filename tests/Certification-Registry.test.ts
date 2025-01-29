import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const contractOwner = accounts.get("deployer")!;
const issuer = accounts.get("wallet_1")!;
const recipient = accounts.get("wallet_2")!;

describe("Certification-Registry contract", () => {
  it("allows contract owner to add authorized issuers", () => {
    const addIssuerCall = simnet.callPublicFn(
      "Certification-Registry",
      "add-authorized-issuer",
      [Cl.principal(issuer)],
      contractOwner
    );
    expect(addIssuerCall.result).toBeOk(Cl.bool(true));
  });

  it("allows authorized issuers to issue certifications", () => {
    // First authorize the issuer
    simnet.callPublicFn(
      "Certification-Registry",
      "add-authorized-issuer",
      [Cl.principal(issuer)],
      contractOwner
    );

    // Then issue certification
    const issueCertCall = simnet.callPublicFn(
      "Certification-Registry",
      "issue-certification",
      [
        Cl.principal(recipient),
        Cl.stringAscii("Full Stack Development"),
        Cl.uint(100), // expiry block height
        Cl.stringAscii("Advanced certification in web development")
      ],
      issuer
    );
    expect(issueCertCall.result).toBeOk(Cl.uint(0));
  });

  it("allows verification of valid certifications", () => {
    // Setup: Add issuer and create certification
    simnet.callPublicFn(
      "Certification-Registry",
      "add-authorized-issuer",
      [Cl.principal(issuer)],
      contractOwner
    );

    simnet.callPublicFn(
      "Certification-Registry",
      "issue-certification",
      [
        Cl.principal(recipient),
        Cl.stringAscii("Full Stack Development"),
        Cl.uint(100),
        Cl.stringAscii("Advanced certification in web development")
      ],
      issuer
    );

    // Verify certification
    const verifyCall = simnet.callReadOnlyFn(
      "Certification-Registry",
      "verify-certification",
      [Cl.uint(0)],
      recipient
    );
    expect(verifyCall.result).toBeOk(Cl.tuple({
      "expiry-date": Cl.uint(100),
      "issue-date": Cl.uint(4),
      "issuer": Cl.principal(issuer),
      "metadata": Cl.stringAscii("Advanced certification in web development"),
      "recipient": Cl.principal(recipient),
      "revoked": Cl.bool(false),
      "skill": Cl.stringAscii("Full Stack Development")
    }));
      });

  it("allows issuer to revoke certification", () => {
    // Setup: Add issuer and create certification
    simnet.callPublicFn(
      "Certification-Registry",
      "add-authorized-issuer",
      [Cl.principal(issuer)],
      contractOwner
    );

    simnet.callPublicFn(
      "Certification-Registry",
      "issue-certification",
      [
        Cl.principal(recipient),
        Cl.stringAscii("Full Stack Development"),
        Cl.uint(100),
        Cl.stringAscii("Advanced certification in web development")
      ],
      issuer
    );

    // Revoke certification
    const revokeCall = simnet.callPublicFn(
      "Certification-Registry",
      "revoke-certification",
      [Cl.uint(0)],
      issuer
    );
    expect(revokeCall.result).toBeOk(Cl.bool(true));
  });
});